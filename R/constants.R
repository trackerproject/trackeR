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
#' @param ... Currently not used
#' @export
generate_base_units <- function(...) {
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
    return(data.frame(variable = varnames, unit = units, stringsAsFactors = FALSE))
}

#' Generate default thresholds
#'
#' @param ... Currently not used
#' @export
generate_thresholds <- function(...) {
    th <- generateBaseUnits()
    n_variables <- nrow(th)
    th <- rbind(th, th, th)
    th$sport <- rep(c("cycling", "running", "swimming"), each = n_variables)
    th$lower <- c(-90, -180, -500, 0, 0, 0, 0, 0, 0, -30, 0, 0)
    th$upper <- c(c(90, 180, 9000, Inf, 250, 10^2, Inf, Inf, Inf, 60, Inf, Inf),
                  c(90, 180, 9000, Inf, 250, 12.5, Inf, Inf, Inf, 60, Inf, Inf),
                  c(90, 180, 9000, Inf, 250, 5, Inf, Inf, Inf, 60, Inf, Inf))
    class(th) <- c("trackeRthresholds", class(th))
    return(th)
}

