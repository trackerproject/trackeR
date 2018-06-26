## Generate variables names for internal use in readX functions. The
## variables vectors need to correspond one by on interms of variable
## type
generate_variable_names <- function() {
    human_names <- c("time",
                     "latitude",
                     "longitude",
                     "altitude",
                     "distance",
                     "heart_rate",
                     "speed",
                     "cadence_running",
                     "cadence_cycling",
                     "power",
                     "temperature")

    tcx_names <- c("Time",
                   "LatitudeDegrees",
                   "LongitudeDegrees",
                   "AltitudeMeters",
                   "DistanceMeters",
                   "HeartRateBpm",
                   "Speed",
                   "RunCadence",
                   "Cadence",
                   "Watts",
                   "temperature")

    gpx_names <- c("time",
                   "lat",
                   "lon",
                   "ele",
                   "distance",
                   "hr",
                   "speed",
                   "rcad", ## dummy for now; gpx seems not to distinguish between run and cycling cadence
                   "cad",
                   "watts",
                   "atemp")

    db3_names <-     c("dttm",
                       "lat",
                       "lon",
                       "altitude",
                       "dist",
                       "hr",
                       "velocity",
                       "rcadence",
                       "cadence", ## dummy for now; db3 seems not to distinguish between run and cycling cadence
                       "watts",
                       "temperature")

    json_names <- c("SECS",
                    "LAT",
                    "LON",
                    "ALT",
                    "KM",
                    "HR",
                    "KPH",
                    "RCAD",
                    "CAD", ## dummy for now; json seems not to distinguish between run and cycling cadence
                    "WATTS",
                    "temperature")
    list(human_names = human_names,
         gpx_names = gpx_names,
         tcx2_names = tcx_names,
         db3_names = db3_names,
         json_names = json_names)
}


#' Generate base units
#'
#' @param variable A vector of variables with user-specified units
#' @param unit A vector with the user-specified units, corresponding
#'     to variable (see details).
#' @param sport A vector of sports (amongst \code{'cycling'},
#'     \code{'running'}, \code{'swimming'}) with each element
#'     corresponding to variable and unit.
#' @param ... Currently not used.
#'
#' @details
#'
#' The avaialble units are
#' \itemize{
#'
#' \item variables \code{latitude} and \code{longitude} with unit
#' \code{degree} (default)
#'
#' \item variables \code{altitude}, \code{distance} with unit \code{m}
#' (default), \code{km}, \code{mi} or \code{ft}
#'
#' \item variable \code{heart_rate} with unit \code{bpm} (default)
#'
#' \item variable \code{speed} with unit \code{m_per_s} (default),
#' \code{km_per_h}, \code{ft_per_min}, \code{ft_per_s} or
#' \code{mi_per_h}
#'
#' \item variable \code{cadence_running} with unit
#' \code{steps_per_min} (default; runing only)
#'
#' \item variable \code{cadence_cycling} with unit \code{rev_per_min}
#' (default; cycling only)
#'
#' \item variable \code{power} with unit \code{W} (Watt; default) or
#' \code{kW} (cycling only)
#'
#' \item variable \code{temperature} with unit \code{C} (Celsius;
#' default) or \code{F}
#' }
#'
#' \code{generate_units} checks if the supplied combinations of
#' \code{variable} and \code{sport} are valid.  If \code{unit} is
#' specified, \code{generate_units} will not check if the supplied
#' units are correct for the cirresponding combination of
#' \code{variable} and \code{sport}.
#'
#' @export
generate_units <- function(variable, unit, sport, ...) {
    ## Get the variable names
    varnames <- generate_variable_names()$human_names
    ## Remove time and add duration
    varnames <- varnames[-match("time", varnames)]
    varnames <- c(varnames, c("pace", "duration"))
    units <- c("degree",
               "degree",
               "m",
               "m",
               "bpm",
               "m_per_s",
               "steps_per_min",
               "rev_per_min",
               "W",
               "C",
               "min_per_km",
               "s")
    sports <- c("cycling", "running", "swimming")
    out <- data.frame(variable = rep(varnames, 3),
                      unit = rep(units, 3),
                      sport = rep(sports, each = length(units)),
                      stringsAsFactors = FALSE)

    ## remove impossible combiations of variables and sports
    inds <- with(out, (sport == "cycling" & variable == "cadence_running") |
                      (sport == "running" & variable == "cadence_cycling") |
                      (sport == "swimming" & variable == "cadence_running") |
                      (sport == "swimming" & variable == "cadence_cycling") |
                      (sport == "running" & variable == "power") |
                      (sport == "swimming" & variable == "power"))
    out <- out[!inds, ]
    no_variable <- missing(variable)
    no_unit <- missing(variable)
    no_sport <- missing(sport)
    if (no_sport & no_unit & no_variable) {
        return(out)
    }
    if (no_sport | no_unit | no_variable) {
        stop("specify variable, unit and sport")
    }
    else {
        p <- length(sport)
        if (length(unit) == p & length(variable) == p) {
            inds <- paste(variable, sport, sep = "_") %in% paste(out$variable, out$sport, sep = "_")

            if (!all(inds)) {
                stop("at least some of the specified combinations of variable and sport are not implemented")
            }
            else {
                for (j in seq.int(p)) {
                    out[out$variable == variable[j] & out$sport == sport[j], "unit"] <- unit[j]
                }
            }
            return(out)
        }
        else {
            stop("variable, unit and sport should have the same length")
        }
    }

}


#' Generate default thresholds
#'
#' @param ... Currently not used.
#' @export
generate_thresholds <- function(variable, lower, upper, sport, ...) {
    th <- generate_units()
    n_variables <- nrow(th)
    th$lower <- c(c(-90, -180, -500, 0, 0, 0, 0, 0, -30, 0, 0), # cycling
                  c(-90, -180, -500, 0, 0, 0, 0, -30, 0, 0), # running
                  c(-90, -180, -500, 0, 0, 0, -30, 0, 0)) # swimming
    th$upper <- c(c(90, 180, 9000, Inf, 250, 10^2, Inf, Inf, 60, Inf, Inf), # cycling
                  c(90, 180, 9000, Inf, 250, 12.5, Inf, 60, Inf, Inf), # running
                  c(90, 180, 9000, Inf, 250, 5, 60, Inf, Inf)) # swimming

    no_variable <- missing(variable)
    no_lower <- missing(lower)
    no_upper <- missing(upper)
    no_sport <- missing(sport)
    if (no_sport & no_lower & no_upper & no_variable) {
        class(th) <- c("trackeRthresholds", class(th))
        return(th)
    }
    if (no_sport | no_lower| no_upper | no_variable) {
        stop("specify variable, lower, upper and sport")
    }
    else {
        p <- length(sport)
        if (length(lower) == p & length(upper) & length(variable) == p) {
            inds <- paste(variable, sport, sep = "_") %in% paste(th$variable, th$sport, sep = "_")
            if (!all(inds)) {
                stop("at least some of the specified combinations of variable and sport are not implemented")
            }
            else {
                for (j in seq.int(p)) {
                    th[th$variable == variable[j] & th$sport == sport[j], "lower"] <- lower[j]
                    th[th$variable == variable[j] & th$sport == sport[j], "upper"] <- upper[j]
                }
            }
        }
        else {
            stop("variable, unit and sport should have the same length")
        }
    }

    class(th) <- c("trackeRthresholds", class(th))
    return(th)
}

