#' Create a trackeRdata object
#'
#' Create a trackeRdata object from a data frame with observations
#' being divided in separate training sessions. For breaks within a
#' session observations are imputed.
#'
#' @aliases trackeRdata
#' @param dat A \code{\link{data.frame}} object.
#' @param units The output of \code{\link{generate_units}}.
#' @param sport What sport does \code{dat} contain data of? Either
#'     \code{'cycling'}, \code{'running'}, \code{'swimming'} or
#'     \code{NULL} (default), in which case the sport is directly
#'     extracted from the \code{dat}. See Details.
#' @param correct_distances Logical. Should the distances be corrected
#'     for elevation?
#' @param country ISO3 country code for downloading altitude data. If
#'     \code{NULL}, country is derived from longitude and latitude
#' @param mask Logical. Passed on to
#'     \code{\link[raster]{getData}}. Should only the altitudes for
#'     the specified \code{country} be extracted (\code{TRUE}) or also
#'     those for the neighboring countries (\code{FALSE})?
#' @inheritParams sanity_checks
#' @inheritParams get_resting_periods
#' @inheritParams impute_speeds
#' @details
#'
#' During small breaks within a session, e.g., because the recording
#' device was paused, observations are imputed the following way: 0
#' for speed, last known position for latitude, longitude and
#' altitude, NA or 0 power for running or cycling session,
#' respectively, and NA for all other variables. Distances are
#' (re-)calculated based on speeds after imputation.
#'
#' \code{trackeRdata} assumes that all observations in \code{dat} are
#' from the same \code{sport}, even if \code{dat} ends up having
#' observations from different sessions (also depending on the value
#' of \code{session_threshold}.
#'
#' if \code{attr(dat, 'sport')} is \code{NA} then the current
#' implementation of \code{trackeRdata} returns an error.
#'
#' @seealso \code{\link{readContainer}} for reading .tcx and .db3
#'     files directly into \code{trackeRdata} objects.
#'
#' @references
#'
#' Frick, H., Kosmidis, I. (2017). trackeR: Infrastructure for Running
#' and Cycling Data from GPS-Enabled Tracking Devices in
#' R. \emph{Journal of Statistical Software}, \bold{82}(7),
#' 1--29. doi:10.18637/jss.v082.i07
#'
#' @examples
#' \dontrun{
#' ## read raw data
#' filepath <- system.file('extdata/tcx/', '2013-06-08-090442.TCX', package = 'trackeR')
#' run <- readTCX(file = filepath, timezone = 'GMT')
#'
#' ## turn into trackeRdata object
#' units0 <- data.frame(variable = c('latitude', 'longitude', 'altitude', 'distance',
#'                                   'heart_rate', 'speed', 'cadence_running', 'cadence_cycling',
#'                                   'power'),
#'                      unit = c('degree', 'degree', 'm', 'm', 'bpm', 'm_per_s', 'steps_per_min',
#'                               'rev_per_min', 'W'),
#'                      stringsAsFactors = FALSE)
#' run <- trackeRdata(run, units = units0)
#'
#' ## alternatively
#' run <- readContainer(filepath, type = 'tcx', timezone = 'GMT')
#' }
#' @export
trackeRdata <- function(dat,
                        units = NULL,
                        sport = NULL,
                        session_threshold = 2,
                        correct_distances = FALSE,
                        from_distances = TRUE,
                        country = NULL,
                        mask = TRUE,
                        lgap = 30,
                        lskip = 5,
                        m = 11,
                        silent = FALSE) {
    ## file
    file <- attr(dat, "file")

    ## sport
    if (is.null(sport)) {
        sport <- attr(dat, "sport")
    }
    else {
        sport <- match.arg(sport, c("cycling", "swimming", "running"))
    }

    ## For now throw error. In future, classify sport if it is NA
    if (is.na(sport)) {
        stop("could not identify the sport from the filename or the data")
    }

    ## prep units
    if (is.null(units)) {
        units <- generate_units()
    }

    ## basic edits on time stamps
    dat <- sanity_checks(dat = dat, silent = silent)

    ## separate sessions and cast to zoo objects
    trackerdat <- get_sessions(dat, session_threshold = session_threshold)

    ## remove sessions which only contain NA
    empty <- sapply(trackerdat, function(x) all(is.na(x)))
    trackerdat <- trackerdat[!empty]

    ## correct GPS distances for elevation
    if (correct_distances) {
        trackerdat <- lapply(trackerdat, distance_correction, country = country, mask = mask)
    }

    ## impute speeds in each session
    trackerdat <- lapply(trackerdat, impute_speeds, from_distances = from_distances, lgap = lgap,
                         lskip = lskip, m = m, sport = sport, units = units)

    ## compute pace and add limits
    pace_inv <- strsplit(units$unit[units$variable == "pace" & units$sport == sport], split = "_per_")[[1]][2:1]
    pace_inv <- paste(pace_inv, collapse = "_per_")
    conversion <- match.fun(paste(units$unit[units$variable == "speed" & units$sport == sport], pace_inv, sep = "2"))
    trackerdat <- lapply(trackerdat, function(x) {
        x$pace <- 1/conversion(x$speed)
        x$pace[is.infinite(x$pace)] <- NA
        return(x)
    })

    ## limits <- lapply(trackerdat, function(sess) {
    ##     get_limits(as.data.frame(sess), a = 0.001)
    ## })
    ## low <- do.call("cbind", lapply(limits, "[[", "lower"))
    ## upp <- do.call("cbind", lapply(limits, "[[", "upper"))
    ## low <- apply(low, 1, function(x) if (all(is.na(x))) NA else min(x, na.rm = TRUE))
    ## upp <- apply(upp, 1, function(x) if (all(is.na(x))) NA else max(x, na.rm = TRUE))

    ## Set attributes
    ## attr(trackerdat, "lower") <- low
    ## attr(trackerdat, "upper") <- upp
    attr(trackerdat, "operations") <- list(smooth = NULL, threshold = NULL)
    attr(trackerdat, "units") <- units
    attr(trackerdat, "sport") <- rep(sport, length(trackerdat))
    attr(trackerdat, "file") <- rep(file, length(trackerdat))


    ## class and return
    class(trackerdat) <- c("trackeRdata", class(trackerdat))
    return(trackerdat)
}

#' @export
c.trackeRdata <- function(...,
                          recursive = FALSE) {
    ## FIXME: recursive argument

    input <- list(...)
    input <- input[!unlist(lapply(input, is.null))]
    ninput <- length(input)
    if (ninput < 2)
        return(input[[1]])
    nsessionsInput <- sapply(input, length)
    units1 <- getUnits(input[[1]])
    operations <- get_operations(input[[1]])

    ## check/change operations attributes: smooth

    ## if all smoother settings are NULL, skip whole aggregation process
    if (!all(sapply(input, function(x) is.null(get_operations(x)$smooth)))) {

        ## if the settings for the first session are NULL, create a new reference setup
        if (is.null(get_operations(input[[1]])$smooth)) {
            operations$smooth <- list(fun = NA, width = NA, parallel = FALSE, cores = NULL,
                                      what = NA, nsessions = NULL)
        }


        funs <- sapply(input, function(x) get_operations(x)$smooth$fun)
        funs <- funs[!sapply(funs, is.null)]
        funs <- funs[!sapply(funs, is.na)]
        if (any(!sapply(funs, function(x) isTRUE(all.equal(funs[[1]], x)))))
            stop("Smoothing function must be the same for all sessions.")
        if (is.na(operations$smooth$fun))
            operations$smooth$fun <- funs[[1]]

        widths <- lapply(input, function(x) unique(get_operations(x)$smooth$width))
        whats <- lapply(input, function(x) unique(get_operations(x)$smooth$what))
        changeWidth <- any(!sapply(widths, function(x) isTRUE(all.equal(widths[[1]], x))))
        changeWhat <- any(!sapply(whats, function(x) isTRUE(all.equal(whats[[1]], x))))
        changeO <- changeWidth | changeWhat
        if (changeO) {
            widths <- lapply(input, function(x) get_operations(x)$smooth$width)
            widths[sapply(widths, is.null)] <- operations$smooth$width[1]
            widths <- do.call("c", widths)
            whats <- lapply(input, function(x) get_operations(x)$smooth$what)
            whats[sapply(whats, is.null)] <- list(operations$smooth$what[1])
            whats <- do.call("c", whats)
            nsessions <- lapply(input, function(x) get_operations(x)$smooth$nsessions)
            nsessions[sapply(nsessions, is.null)] <- nsessionsInput[sapply(nsessions, is.null)]
            nsessions <- do.call("c", nsessions)
            operations$smooth$width <- widths
            operations$smooth$what <- whats
            operations$smooth$nsessions <- nsessions
        }
        else {
            nsessions <- lapply(input, function(x) get_operations(x)$smooth$nsessions)
            nsessions[sapply(nsessions, is.null)] <- nsessionsInput[sapply(nsessions, is.null)]
            operations$smooth$nsessions <- sum(do.call("c", nsessions))
        }
    }

    ## check/change operations attributes: threshold apply thresholds of first session to
    ## all sessions if necessary
    th <- operations$threshold
    thAll <- lapply(input, function(x) get_operations(x)$threshold)
    changeT <- !all(sapply(thAll, function(x) isTRUE(all.equal(th, x))))
    if (changeT) {
        if (is.null(th)) {
            warning("The first session does not have any thresholds, this is applied to all sessions.")
        } else {
            warning("The sessions have different thresholds. The thresholds of the first session are applied to all sessions.")
        }
        ## change thresholds
        for (i in 2:ninput) {
            input[[i]] <- threshold(input[[i]], th$variable, th$lower, th$upper, th$sport)
        }
    }

    ## check/change units attribute
    units <- lapply(input, getUnits)

    changeU <- !all(sapply(units, function(x) isTRUE(all.equal(units[[1]], x))))
    if (changeU) {
        warning("The sessions have different units. The units from the first session have been applied to all sessions.")
        ## change units
        for (i in 2:ninput) {
            input[[i]] <- change_units(input[[i]], variable = units1$variable, unit = units1$unit, sport = units1$sport)
        }
    }

    ## combine sessions
    ret <- vector("list", sum(nsessionsInput))
    files0 <- sapply(input, attr, which = "file")
    files <- character(sum(nsessionsInput))
    starti <- c(1, cumsum(nsessionsInput)[-length(nsessionsInput)] + 1)
    endi <- cumsum(nsessionsInput)

    for (i in seq_len(ninput)) {
        ret[starti[i]:endi[i]] <- input[[i]]
        files[starti[i]:endi[i]] <- files0[[i]]
    }

    ## ## merge limits
    ## low <- sapply(input, attr, which = "lower")
    ## upp <- sapply(input, attr, which = "upper")
    ## low <- apply(low, 1, function(x) if (all(is.na(x))) NA else min(x, na.rm = TRUE))
    ## upp <- apply(upp, 1, function(x) if (all(is.na(x))) NA else max(x, na.rm = TRUE))

    ## class and other attributes
    class(ret) <- c("trackeRdata", "list")
    files <- unlist(sapply(input, attr, which = "file"))
    attr(ret, "units") <- units1
    attr(ret, "sport") <- unlist(sapply(input, attr, which = "sport"))
    attr(ret, "file") <- files
    ## operations$smooth
    attr(ret, "operations") <- operations

    return(ret)

}

#' Sort sessions in \code{\link{trackeRdata}} objects
#'
#' Sort the sessions \code{\link{trackeRdata}} objects into ascending
#' or descending order according to the first session timestamp.
#'
#' @param x A \code{trackeRdata} object.
#' @param decreasing Logical. Should the objects be sorted in
#'     increasing or decreasing order?
#' @param ... Currently not used.
#'
#' @export
sort.trackeRdata <- function(x,
                             decreasing = FALSE,
                             ...) {
    oo <- order(sapply(x, function(session) index(session)[1]))
    if (decreasing) {
        ret <- x[rev(oo)]
    }
    else {
        ret <- x[oo]
    }
    ret
}

#' Exrtact unique sessions in a \code{trackerRdata} object
#'
#' @param x A \code{trackeRdata} object.
#' @param incomparables Currently not used.
#' @param ... Currently not used.
#'
#' @details
#'
#' Uniqueness is determined by comparing the first timestamp of the
#' sessions in the \code{trackeRdata} object.
#'
#' @export
unique.trackeRdata <- function(x,
                               incomparables = FALSE,
                               ...) {
    ## NOTE: Consider determining uniqueness according to file name?
    start <- sapply(x, function(session) index(session)[1])
    inds <- !duplicated(start, incomparables = FALSE)
    ret <- x[inds]
    ret
}


#' @export
"[.trackeRdata" <- function(x, i, j, drop = TRUE, ...) {

    units <- getUnits(x)
    operations <- get_operations(x)
    sport <- attr(x, "sport")
    files <- attr(x, "file")

    ret <- NextMethod()
    is_null <- sapply(ret, is.null)
    if (any(is_null)) {
        stop("Subsetting failed; non-existing sessions: ", paste(i[is_null], collapse = ", "))
    }

    if (!is.null(operations$smooth)) {
        smooth <- operations$smooth
        ## select right smoothing parameters for the i session(s)

        ## elements j from smooting settings
        j <- rep(seq_along(smooth$nsessions), times = smooth$nsessions)[i]

        if (length(j) < 2) {
            k <- j
            nsessions <- length(j)
        }
        else {
            ## to avoid duplicating unnecessary information, aggregate j to k and keep track of
            ## number of sessions NOTE: k <- unique(j) ; smooth$nsessions <- as.numeric(table(j))
            ## does not allow to split sessions from one block - but x[i] does allow it.  Thus the
            ## following aggregation to k and nsessions:
            counter <- breakpoints <- rep(NA, length(j))
            counter[1] <- 1
            breakpoints[1] <- TRUE
            for (a in 2:length(j)) {
                if (j[a] == j[a - 1]) {
                  counter[a] <- counter[a - 1] + 1
                  breakpoints[a] <- FALSE
                } else {
                  counter[a] <- 1
                  breakpoints[a] <- TRUE
                }
            }
            ## cbind(j, counter, breakpoints)
            k <- j[breakpoints]
            nsessions <- counter[c(which(breakpoints)[-1] - 1, length(j))]
        }

        smooth$width <- smooth$width[k]
        smooth$what <- smooth$what[k]
        smooth$nsessions <- nsessions
        operations$smooth <- smooth
    }

    ## class and attributes
    class(ret) <- c("trackeRdata", "list")
    attr(ret, "lower") <- attr(x, "lower")
    attr(ret, "upper") <- attr(x, "upper")
    attr(ret, "units") <- units
    attr(ret, "operations") <- operations
    attr(ret, "sport") <- sport[i]
    attr(ret, "file") <- files[i]

    return(ret)
}


#' Append training sessions to existing file
#'
#' @param object The object to be appended.
#' @param file The file to which \code{object} is to be appended.
#' @param ... Currently not used.
#' @export
append.trackeRdata <- function(object,
                               file,
                               ...) {
    old <- load(file)
    new <- c(old, object)
    save(new, file)
}

#' @export
nsessions.trackeRdata <- function(object,
                                  ...) {
    length(object)
}


#' Coercion function for use in Golden Cheetah
#'
#' @param gc Output of \code{GC.activity}.
#' @param cycling Logical. Does the data stem from cycling?
#' @inheritParams trackeRdata
#' @inheritParams sanity_checks
#' @inheritParams get_resting_periods
#' @inheritParams impute_speeds
#' @seealso \code{\link{trackeRdata}}
#' @export
GC2trackeRdata <- function(gc,
                           cycling = TRUE,
                           correct_distances = FALSE,
                           country = NULL,
                           mask = TRUE,
                           from_distances = FALSE,
                           lgap = 30,
                           lskip = 5,
                           m = 11,
                           silent = FALSE) {

    units <- data.frame(
        variable = c("latitude", "longitude", "altitude", "distance", "heart_rate",
                     "speed", "cadence_running", "cadence_cycling", "power", "pace"),
        unit = c("degree", "degree", "m", "km", "bpm",
                 "km_per_h", "rev_per_min", "steps_per_min", "W", "min_per_km"), stringsAsFactors = FALSE)

    ## clear out sessions without any data
    gc <- gc[sapply(gc, function(x) nrow(x) > 0)]

    ## get variables, cast to zoo
    trackerdat <- lapply(gc, function(x) {
        ## select variables
        x <- x[, c("time", "latitude", "longitude", "altitude", "distance", "heart_rate",
            "speed", "cadence_running", "cadence_cycling", "power")]

        ## basic edits
        x <- sanity_checks(dat = x, silent = silent)
        ## README: add arg sort = T/F to sanity_checks() so we don't need to sort the
        ## observations again if we can be sure that GC already does this

        ## cast to multivariate zoo
        wtime <- which(names(x) == "time")
        x <- zoo(x[, -wtime], order.by = x[, "time"])
    })

    ## remove sessions which only contain NA
    empty <- sapply(trackerdat, function(x) is.null(x) | all(is.na(x)))
    trackerdat <- trackerdat[!empty]

    ## correct GPS distances for elevation
    if (correct_distances)
        trackerdat <- lapply(trackerdat, distance_correction, country = country, mask = mask)

    ## impute speeds in each session
    trackerdat <- lapply(trackerdat, impute_speeds, from_distances = from_distances, lgap = lgap,
        lskip = lskip, m = m, cycling = cycling, units = units)

    ## add pace
    trackerdat <- lapply(trackerdat, function(x) {
        x$pace <- 1/km_per_h2km_per_min(x$speed)
        x$pace[is.infinite(x$pace)] <- NA
        return(x)
    })

    ## Set attributes
    attr(trackerdat, "operations") <- list(smooth = NULL, threshold = NULL)
    attr(trackerdat, "units") <- units

    ## class and return
    class(trackerdat) <- c("trackeRdata", class(trackerdat))
    return(trackerdat)

}



## as.data.frame(x, row.names = NULL, optional = FALSE, ...)
#' @export
as.data.frame.trackeRdata <- function(x,
                                      ...) {

    ret <- vector(length = length(x), "list")

    for (i in seq_along(x)) {
        ret[[i]] <- cbind(session = i, time = index(x[[i]]), as.data.frame(x[[i]]))
    }

    ret <- do.call(rbind, ret)

    return(ret)
}

#' \code{\link{print}} method for \code{\link{trackeRdata}} objects
#'
#' @param x An object of class \code{\link{trackeRdata}}.
#' @param digits Number of digits to be printed.
#' @param duration_unit The unit of duration in the resulting output. Default is \code{h} (hours).
#' @param ... Currently not used; only for compatibility with generic \code{\link{summary}} method only.
#'
#' @details
#'
#' The print method returns training coverage, number of sessions and
#' total training duration from the data in the
#' \code{\link{trackeRdata}} object.
#'
#' @export
print.trackeRdata <- function(x,
                              duration_unit = "h",
                              digits = 2,
                              ...) {
    units <- getUnits(x)
    times <- session_times(x)
    d <- session_duration(x, duration_unit = duration_unit)
    cat("A trackeRdata object\n")
    cat("Sports:", unique(get_sport(x)), "\n\n")
    cat("Training coverage:",
        "from", format(min(times$sessionStart), format = "%Y-%m-%d %H:%M:%S"),
        "to", format(max(times$sessionEnd), format = "%Y-%m-%d %H:%M:%S"), "\n")
    cat("Number of sessions:", nsessions(x), "\n")
    cat("Training duration:", round(as.numeric(sum(d)), digits), duration_unit, "\n\n")

    cat("Units\n")
    colnames(units) <- NULL
    print(units, row.names = FALSE, right = FALSE)
}

#' @rdname session_times
#' @export
session_times.trackeRdata <- function(object,
                                      session = NULL,
                                      ...) {
    if (is.null(session)) {
        session <- seq_along(object)
    }
    out <- data.frame(sessionStart = as.POSIXct(sapply(object, function(x) min(index(x))),
                                                origin = "1970-01-01"),
                      sessionEnd = as.POSIXct(sapply(object, function(x) max(index(x))),
                                              origin = "1970-01-01"))
    out[session, ]
}

#' @rdname session_duration
#' @export
session_duration.trackeRdata <- function(object,
                                         session = NULL,
                                         duration_unit = "h",
                                         ...) {
    ## Match units to those of unit_reference_sport
    ## units <- get_units(object)
    ## if (is.null(unit_reference_sport)) {
    ##     unit_reference_sport <- find_unit_reference_sport(object)
    ## }
    ## un <- collect_units(units, unit_reference_sport)
    ## ## Get duration unit
    ## duration_unit <- un$unit[un$variable == "duration"]
    du <- switch(duration_unit, "s" = "secs", "min" = "mins", "h" = "hours", "d" = "days")

    with(session_times(object, session = session), {
        difftime(sessionEnd, sessionStart, units = du)
    })
}

#' @rdname get_sport
#' @export
get_sport.trackeRdata <- function(object,
                                  session = NULL,
                                  ...) {
    if (is.null(session)) {
        session <- seq_along(object)
    }
    attr(object, "sport")[session]
}

#' Get the units of the variables in an \code{trackeRdata} object
#'
#' @param object An object of class \code{\link{trackeRdata}}.
#' @param ... Currently not used.
#' @export
get_units.trackeRdata <- function(object, ...) {
    attr(object, "units")
}

#' Change the units of the variables in an \code{trackeRdata} object
#'
#' @param object An object of class \code{\link{trackeRdata}}.
#' @inheritParams change_units
#' @export
change_units.trackeRdata <- function(object,
                                     variable,
                                     unit,
                                     sport,
                                     ...) {
    ## get current units and thresholds
    units <- get_units(object)
    operations <- get_operations(object)
    sports <- get_sport(object)

    is_na_sports <- is.na(sport)
    if (any(is.na(sports))) {
        stop("cannot change units. The sport for sessions", which(is_na_sports), "has not been identified. See ?set_sport on how to set a sport for those sessions.")
    }

    th <- operations$threshold

    no_variable <- missing(variable)
    no_unit <- missing(unit)
    no_sport <- missing(sport)

    if (no_sport & no_unit & no_variable) {
        return(object)
    }
    else {
        p <- length(sport)
        if (length(unit) == p & length(variable) == p) {
            inputs <- data.frame(sport = sport, variable = variable, unit = unit, stringsAsFactors = FALSE)
            inds <- match(paste(inputs$sport, inputs$variable, sep = "-"),
                          paste(units$sport, units$variable, sep = "-"),
                          nomatch = 0)
            units$new_unit <- units$unit
            ## If variable/sport/units combinations do not exist then the object is returned
            if (all(inds == 0)) {
                stop("some of the supplied combinations of sport and variable do not exist.")
            }

            units$new_unit[inds] <- inputs$unit
            units$fun <- paste(units$unit, units$new_unit, sep = "2")
            units$changed <- units$unit != units$new_unit

            ## Check for crappy units
            ch <- sapply(units$fun, match.fun)

            for (sp in unique(sports)) {
                un <- subset(units, sport == sp)
                for (k in which(un$changed)) {
                    convert <- match.fun(un$fun[k])
                    va <- un$variable[k]
                    ## Do thresholds if they exist
                    if (!is.null(th)) {
                        th[th$sport == sp & th$variable == va, "lower"] <-
                            convert(th[th$sport == sp & th$variable == va, "lower"])
                        th[th$sport == sp & th$variable == va, "upper"] <-
                            convert(th[th$sport == sp & th$variable == va, "upper"])
                        th[th$sport == sp & th$variable == va, "unit"] <-
                            un$new_unit[k]
                    }
                    ## trackeRdata objects do not carry duration so skip
                    if (va == "duration") {
                        next
                    }
                    for (sess in which(sports == sp)) {
                        object[[sess]][, va] <- convert(object[[sess]][, va])
                    }
                }
            }

            ## Clean up units
            units$unit <- units$new_unit
            units$fun <- units$new_unit <- units$changed <- NULL

            ## update attributes and return
            attr(object, "units") <- units
            if (!is.null(th)) {
                operations$threshold <- th
            }

            attr(object, "operations") <- operations
            return(object)

        }
        else {
            stop("variable, unit and sport should have the same length.")
        }
    }
}


#' Get the operation settings of an \code{trackeRdata} object
#'
#' @param object An object of class \code{\link{trackeRdata}}.
#' @param ... Currently not used.
#' @export
get_operations.trackeRdata <- function(object, ...) {
    attr(object, "operations")
}
