#' Summary of training sessions
#'
#' @aliases trackeRdataSummary
#' @param object An object of class \code{\link{trackeRdata}}.
#' @param session A numeric vector of the sessions to be summarised,
#'     defaults to all sessions.
#' @param moving_threshold A named vector of 3 speeds above which an
#'     athlete is considered moving, given in the unit of the speed
#'     measurements in \code{object}. If \code{NULL} (default), the
#'     speeds are taken to be \code{c(cycling = 2, running = 1,
#'     swimming = 0.5)}. See Details.
#' @param unit_reference_sport The sport to inherit units from
#'     (default is taken to be the most frequent sport in
#'     \code{object}).
#' @param ... Currently not used.
#'
#' @details
#'
#' The default speed thresholds are 1 m/s for running (3.6 km/h; slow
#' walking), 2 m/s for cycling (7.2 km/h) for cycling and 0.5 m/s
#' (1.8km/h) for swimming. For reference, the preferred walking speed
#' for humans is around 1.4 m/s (Bohannon, 1997).
#'
#' The units for the computed summaries match those of the sport
#' specified by \code{unit_reference_sport}.
#'
#' If \code{object} has thresholds then the thresholds that match
#' those of the sport specified by \code{unit_reference_sport} are
#' applied to the respective summaries.
#'
#' @return
#'
#' An object of class \code{trackeRdataSummary}.
#'
#'
#' @seealso \code{\link{plot.trackeRdataSummary}}
#' @references
#'
#' Bohannon RW (1997). 'Comfortable and Maximum Walking Speed of
#' Adults Aged 20--79 Years: Reference Values and Determinants.' Age
#' and Ageing, 26(1), 15--19. doi: 10.1093/ageing/26.1.15.
#'
#' @examples
#' data('runs', package = 'trackeR')
#' runSummary <- summary(runs, session = 1:2)
#' ## print summary
#' runSummary
#' print(runSummary, digits = 3)
#' ## change units
#' change_units(runSummary, variable = 'speed', unit = 'km_per_h')
#' ## plot summary
#' runSummaryFull <- summary(runs)
#' plot(runSummaryFull)
#' plot(runSummaryFull, group = c('total', 'moving'),
#'     what = c('avgSpeed', 'distance', 'duration', 'avgHeartRate'))
#' @export
summary.trackeRdata <- function(object,
                                session = NULL,
                                moving_threshold = NULL,
                                unit_reference_sport = NULL,
                                ...) {
    units <- get_units(object)
    sports <- get_sport(object)
    oper <- get_operations(object)
    thres <- oper$threshold

    files <- attr(object, "file")

    if (is.null(unit_reference_sport)) {
        unit_reference_sport <- find_unit_reference_sport(object)
    }

    ## Match units to those of unit_reference_sport
    un <- collect_units(units, unit_reference_sport)
    for (va in unique(un$variable)) {
        units$unit[units$variable == va] <- un$unit[un$variable == va]
    }

    ## Change threshold units
    if (!is.null(thres)) {
        thres <- change_units(thres, units$variable, units$unit, units$sport)
        ## Uniformize thresholds (Check again if that's indeed what we need here)
        for (va in unique(un$variable)) {
            inds <- thres$variable == va & thres$sport == unit_reference_sport
            if (any(inds)) {
                thres$lower[thres$variable == va] <- thres$lower[inds]
                thres$upper[thres$variable == va] <- thres$upper[inds]
            }
        }
        oper$threshold <- thres
    }


    ## convert moving_threshold
    if (is.null(moving_threshold)) {
        moving_threshold <- c(cycling = 2, running = 1, swimming = 0.5)
        speed_unit <- un$unit[un$variable == "speed"]
        if (speed_unit != "m_per_s") {
            conversion <- match.fun(paste("m_per_s", speed_unit, sep = "2"))
            moving_threshold <- conversion(moving_threshold)
        }
    }

    ## select sessions
    if (is.null(session)) {
        session <- seq.int(length(object))
    }

    object <- object[session]

    ## Change units to those of unit_reference_sport
    object <- changeUnits(object, units$variable, units$unit, units$sport)

    ## session times
    session_start <- as.POSIXct(sapply(object, function(x) min(index(x))), origin = "1970-01-01")
    session_end <- as.POSIXct(sapply(object, function(x) max(index(x))), origin = "1970-01-01")

    ## distance
    distance <- sapply(object, function(x) {
        d <- zoo::coredata(x$distance)
        if (all(is.na(d))) {
            return(NA)
        }
        else {
            max(d, na.rm = TRUE)
        }
    })

    ## session length (unit set by units)
    duration_unit <- un$unit[un$variable == "duration"]
    du <- switch(duration_unit, "s" = "secs", "min" = "mins", "h" = "hours", "d" = "days")
    duration <- difftime(session_end, session_start, units = du)

    ## Get session durations moving and convert their units to duration_unit
    duration_moving <- lapply(session, function(sess) {
        sp <- sports[sess]
        out <- timeAboveThreshold(object[[sess]]$speed, threshold = moving_threshold[sp], ge = FALSE)
        units(out) <- du
        out
    })
    duration_moving <- do.call("c", duration_moving)

    ## average speed
    distance_unit <- un$unit[un$variable == "distance"]
    speed_unit <- strsplit(units$unit[un$variable == "speed"], split = "_per_")[[1]]
    convert_distance <- match.fun(paste(distance_unit, speed_unit[1], sep = "2"))
    distance_s <- convert_distance(distance)
    convert_duration <- match.fun(paste(duration_unit, speed_unit[2], sep = "2"))
    duration_s <- convert_duration(as.numeric(duration))
    avg_speed <- distance_s/duration_s

    ## average speed moving
    duration_moving_s <- convert_duration(as.numeric(duration_moving))
    avg_speed_moving <- distance_s/duration_moving_s

    ## average pace
    pace_unit <- strsplit(un$unit[un$variable == "pace"], split = "_per_")[[1]]
    convert_distance <- match.fun(paste(distance_unit, pace_unit[2], sep = "2"))
    distance_p <- convert_distance(distance)
    convert_duration <- match.fun(paste(duration_unit, pace_unit[1], sep = "2"))
    duration_p <- convert_duration(as.numeric(duration))
    avg_pace <- duration_p/distance_p


    ## average pace moving
    duration_moving_p <- convert_duration(as.numeric(duration_moving))
    avg_pace_moving <- duration_moving_p/distance_p

    ## work to rest ratio (rest time is duration - duration_moving)
    work2rest <- as.numeric(duration_moving)/as.numeric(duration - duration_moving)

    weightedMeans <- function(x, th, which) {
        n <- nrow(x)
        z <- coredata(x)[-n, c(which, "speed")]
        p <- ncol(z)
        nams <- colnames(z)
        dt <- as.numeric(diff(index(x)))
        i <- 1 - is.na(z)
        m <- as.numeric(z[, "speed"] > th)
        w <- dt * i
        w_moving <- w * m
        w_resting <- w * (1 - m)
        overall <- .colSums(z * w, n - 1, p, na.rm = TRUE) / .colSums(w, n - 1, p, na.rm = TRUE)
        moving <- .colSums(z * w_moving, n - 1, p, na.rm = TRUE) / .colSums(w_moving, n - 1, p, na.rm = TRUE)
        resting <- .colSums(z * w_resting, n - 1, p, na.rm = TRUE) / .colSums(w_resting, n - 1, p, na.rm = TRUE)
        names(overall) <- nams
        names(moving) <- paste0(nams, "_moving")
        names(resting) <- paste0(nams, "_resting")
        ret <- c(overall, moving, resting)
        ret[is.na(ret)] <- NA
        ret
    }

    summaries <- sapply(seq_along(object), function(j) {
        sp <- sports[j]
        weightedMeans(object[[j]],
                      which = c("cadence_running", "cadence_cycling", "power", "heart_rate", "altitude", "temperature"),
                      th = moving_threshold[sp])
    })


    ## ADD: maxima in addition to averages?  calories?  splits per km?

    ret <- data.frame(session = session,
                      sessionStart = session_start,
                      sessionEnd = session_end,
                      distance = distance,
                      duration = duration,
                      durationMoving = duration_moving,
                      avgSpeed = avg_speed,
                      avgSpeedMoving = avg_speed_moving,
                      avgPace = avg_pace,
                      avgPaceMoving = avg_pace_moving,
                      avgCadenceRunning = summaries["cadence_running", ],
                      avgCadenceCycling = summaries["cadence_cycling", ],
                      avgAltitude = summaries["altitude", ],
                      avgAltitudeMoving = summaries["altitude_moving", ],
                      avgCadenceRunningMoving = summaries["cadence_running_moving", ],
                      avgCadenceCyclingMoving = summaries["cadence_cycling_moving", ],
                      avgPower = summaries["power", ],
                      avgPowerMoving = summaries["power_moving", ],
                      avgHeartRate = summaries["heart_rate", ],
                      avgTemperature = summaries["temperature", ],
                      avgHeartRateMoving = summaries["heart_rate_moving", ],
                      avgHeartRateResting = summaries["heart_rate_resting", ],
                      wrRatio = work2rest,
                      sport = sports[session],
                      file = files[session], stringsAsFactors = FALSE)


    ## Replace inf and NaN with NA
    ret[sapply(ret, function(x) is.infinite(x) | is.na(x))] <- NA

    ## Apply thresholds
    lims <- unique(thres[thres$variable %in% un$variable, c("variable", "lower", "upper")])
    for (j in seq.int(nrow(lims))) {
        low <- lims[j, "lower"]
        upp <- lims[j, "upper"]
        cvar <- lims[j, "variable"]
        vars <- grep(cvar, names(ret), ignore.case = TRUE)
        ret[, vars] <- sapply(ret[, vars], function(x) {
            x[x < low | x > upp] <- NA
            x
        })
    }

    attr(ret, "operations") <- oper
    attr(ret, "units") <- units
    attr(ret, "moving_threshold") <- moving_threshold
    attr(ret, "unit_reference_sport") <- attr(un, "unit_reference_sport")
    class(ret) <- c("trackeRdataSummary", class(ret))
    return(ret)
}



#' Print method for session summaries.
#'
#' @param x An object of class \code{trackeRdataSummary}.
#' @param ... Not used, for compatibility with generic summary method only.
#' @param digits Number of digits to be printed.
#' @export
print.trackeRdataSummary <- function(x, ..., digits = 2) {
    units <- get_units(x)
    units <- collect_units(units, unit_reference_sport = attr(x, "unit_reference_sport"))
    sports <- get_sport(x)

    for (i in seq_len(length(x$session))) {
        cat("\n *** Session", x$session[i], ":", sports[i], "***\n")

        cat("\n Session times:",
            format(x$sessionStart[i], format = "%Y-%m-%d %H:%M:%S"),
            "-",
            format(x$sessionEnd[i], format = "%Y-%m-%d %H:%M:%S"), "\n ")

        cat("Distance:",
            round(x$distance[i], digits), units$unit[units$variable == "distance"],
            "\n ")

        cat("Duration:",
            round(as.numeric(x$duration[i]), digits), units(x$duration[i]),
            "\n ")

        cat("Moving time:",
            round(x$durationMoving[i], digits), units(x$durationMoving[i]),
            "\n ")

        cat("Average speed:",
            round(x$avgSpeed[i], digits = digits), units$unit[units$variable == "speed"],
            "\n ")

        cat("Average speed moving:",
            round(x$avgSpeedMoving[i], digits = digits), units$unit[units$variable == "speed"],
            "\n ")

        unitDist4pace <- strsplit(units$unit[units$variable == "pace"], split = "_per_")[[1]][2]
        avgPace <- floor(x$avgPace[i] * 100)/100
        cat(paste0("Average pace (per 1 ", unitDist4pace, "):"), paste(floor(avgPace),
            round(avgPace%%1 * 60, 0), sep = ":"), "min:sec\n ")

        avgPaceMoving <- floor(x$avgPaceMoving[i] * 100)/100
        cat(paste0("Average pace moving (per 1 ", unitDist4pace, "):"), paste(floor(avgPaceMoving),
            round(x$avgPaceMoving[i]%%1 * 60, 0), sep = ":"), "min:sec\n ")

        cat("Average cadence running:", round(x$avgCadenceRunning[i], digits = digits),
            units$unit[units$variable == "cadence_running"], "\n ")
        cat("Average cadence cycling:", round(x$avgCadenceCycling[i], digits = digits),
            units$unit[units$variable == "cadence_cycling"], "\n ")

        cat("Average cadence running moving:", round(x$avgCadenceRunningMoving[i], digits = digits),
            units$unit[units$variable == "cadence_running"], "\n ")
        cat("Average cadence cycling moving:", round(x$avgCadenceCyclingMoving[i], digits = digits),
            units$unit[units$variable == "cadence_cycling"], "\n ")

        cat("Average power:", round(x$avgPower[i], digits = digits), units$unit[units$variable ==
            "power"], "\n ")

        cat("Average power moving:", round(x$avgPowerMoving[i], digits = digits), units$unit[units$variable ==
            "power"], "\n ")

        cat("Average heart rate:", round(x$avgHeartRate[i], digits = digits), units$unit[units$variable ==
            "heart_rate"], "\n ")

        cat("Average heart rate moving:", round(x$avgHeartRateMoving[i], digits = digits),
            units$unit[units$variable == "heart_rate"], "\n ")

        cat("Average heart rate resting:", round(x$avgHeartRateResting[i], digits = digits),
            units$unit[units$variable == "heart_rate"], "\n ")

        cat("Average temperature:", round(x$avgTemperature[i], digits = digits), units$unit[units$variable ==
            "temperature"], "\n ")

        cat("Work to rest ratio:", round(x$wrRatio[i], digits), "\n")
    }
    mt <- attr(x, "moving_threshold")
    cat("\n Moving thresholds:",
        paste0(format(mt, digits = digits), " (", names(mt), ")"),
        units$unit[units$variable == "speed"], "\n")

    cat(" Unit reference sport:",
        attr(x, "unit_reference_sport"), "\n")

    cat("\n")
}


#' Fortify a trackeRdataSummary object for plotting with ggplot2.
#'
#' @param  model The \code{\link{trackeRdata}} object.
#' @param data Ignored.
#' @param melt Logical. Should the data be melted into long format
#'     instead of the default wide format?
#' @param ... Currently not used.
#' @export
fortify.trackeRdataSummary <- function(model, data, melt = FALSE, ...) {
    ret <- data.frame(model)

    if (melt) {

        basic <- ret[, c("session", "sessionStart", "sessionEnd")]

        varsTotal <- c("distance", "duration", "avgSpeed", "avgPace", "avgCadenceRunning",
                       "avgCadenceCycling", "avgPower", "avgHeartRate", "wrRatio")
        varsMoving <- c("duration", "avgSpeed", "avgPace", "avgCadenceRunning",
                        "avgCadenceCycling",
                        "avgPower", "avgHeartRate")
        varsResting <- c("avgHeartRate")

        dfTotal <- data.frame(basic[rep(seq_along(ret$session), times = length(varsTotal)),
            ], variable = rep(varsTotal, each = nrow(ret)), value = unlist(ret[, varsTotal]),
            type = "total")
        dfMoving <- data.frame(basic[rep(seq_along(ret$session), times = length(varsMoving)),
            ], variable = rep(varsMoving, each = nrow(ret)), value = unlist(ret[, paste0(varsMoving,
            "Moving")]), type = "moving")
        dfResting <- data.frame(basic[rep(seq_along(ret$session), times = length(varsResting)),
            ], variable = rep(varsResting, each = nrow(ret)), value = unlist(ret[, paste0(varsResting,
            "Resting")]), type = "resting")

        ret <- rbind(dfTotal, dfMoving, dfResting)
    }
    return(ret)
}


#' Plot an object of class \code{\link{trackeRdataSummary}}.
#'
#' @param x An object of class \code{trackeRdataSummary}.
#' @param date Should the date or the session number be used on the abscissa?
#' @param what Name of variables which should be plotted. Default is all.
#' @param group Which group of variables should be plotted? This can either be
#'     \code{total} or \code{moving}. Default is both.
#' @param trend Should a smooth trend be plotted?
#' @param ... Currently not used.
#' @seealso \code{\link{summary.trackeRdata}}
#' @examples
#' data('runs', package = 'trackeR')
#' runSummary <- summary(runs)
#' plot(runSummary)
#' plot(runSummary, date = FALSE, group = 'total',
#'     what = c('distance', 'duration', 'avgSpeed'))
#' @export
plot.trackeRdataSummary <- function(x,
                                    date = TRUE,
                                    what = NULL,
                                    group = NULL,
                                    trend = TRUE,
                                    ...) {

    nsessions <- length(unique(x$session))
    ndates <- length(unique(x$sessionStart))
    units <- getUnits(x)
    units <- collect_units(units, unit_reference_sport = attr(x, "unit_reference_sport"))

    ## subsets on variables and type
    dat <- fortify(x, melt = TRUE)
    if (!is.null(what)) {
        dat <- subset(dat, variable %in% what)
    }
    if (!is.null(group)) {
        dat <- subset(dat, type %in% group)
    }

    ## remove empty factor levels
    dat$variable <- factor(dat$variable)
    # dat$type <- factor(dat$type)

    ## clean up: if there are only NA observations for a variable, the (free) y-scale cannot
    ## be determined
    empty <- tapply(dat$value, dat$variable, function(x) all(is.na(x)))
    if (any(empty))
        dat <- subset(dat, !(variable %in% names(empty)[empty]))

    ## single session
    if (nsessions < 2) {
        dat$sessionStart <- format(dat$sessionStart, format = "%Y-%m-%d")
        dat$session <- factor(dat$session)
    }

    ## x axis
    if (date) {
        dat$xaxis <- dat$sessionStart
        xlab <- "Date"
    }
    else {
        dat$xaxis <- dat$session
        xlab <- "Session"
    }

    ## (basic) plot
    p <- ggplot(dat)
    if (date & ndates < nsessions)
        stop("All sessions must have unique starting times. Try date = FALSE instead.")

    ## color palette comes from colorspace::rainbow_hcl(3, c = 70)[c(2,1,3)] [1] '#5EB3F0'
    ## '#F68BA2' '#76BD58' an alternative from
    ## http://colorbrewer2.org/#type=qualitative&scheme=Dark2&n=3
    ## scale_colour_manual(values = c('total' = '#1b9e77', 'moving' = '#d95f02',
    ## 'resting' = '#7570b3'))

    ## possibly add lines for 2 or more sessions
    if (nsessions > 1) {
        if (trend) {
            p <- p + geom_line(stat = "smooth",
                               method = "gam",
                               formula = y ~ s(x, bs = "cs", k = 5),
                               aes_(x = quote(xaxis), y = quote(value), color = quote(type)),
                               alpha = 0.5, size = 1,
                               se = FALSE,
                               na.rm = TRUE)
        }
    }

    p <- p +
        geom_point(aes_(x = quote(xaxis), y = quote(value), color = quote(type)), alpha = 0.75, na.rm = TRUE) +
        labs(x = xlab, y = "") +
        ## guides(color = guide_legend(title = "Type")) +
        scale_colour_manual(values = c(total = "#76BD58", moving = "#F68BA2", resting = "#5EB3F0"))

    ## facets
    lab_sum <- function(series) {
        series <- as.character(series)
        concept <- switch(series, avgPace = "pace", avgSpeed = "speed", distance = "distance",
                          duration = "duration", avgPower = "power", avgCadenceRunning = "cadence_running",
                          avgCadenceCycling = "cadence_cycling",
                          avgHeartRate = "heart_rate")
        thisunit <- units$unit[units$variable == concept]
        prettyUnit <- prettifyUnits(thisunit)
        ret <- switch(series,
                      distance = paste0("distance \n [", prettyUnit, "]"),
                      duration = paste0("duration \n [", prettyUnit, "]"),
                      avgSpeed = paste0("avg. speed \n [", prettyUnit, "]"),
                      avgPace = paste0("avg. pace \n [", prettyUnit, "]"),
                      avgCadenceRunning = paste0("avg. cadence \n [", prettyUnit, "]"),
                      avgCadenceCycling = paste0("avg. cadence \n [", prettyUnit, "]"),
                      avgPower = paste0("avg. power \n [", prettyUnit, "]"),
                      avgHeartRate = paste0("avg. heart rate \n [", prettyUnit, "]"),
                      wrRatio = "work-to-rest \n ratio")
        ret
    }
    lab_sum <- Vectorize(lab_sum)

    p <- p +
        facet_grid(facets = "variable ~ .", scales = "free_y", labeller = labeller(variable = lab_sum))  ## +

    ## add bw theme and position of legend
    p <- p + theme_bw() + theme(legend.position = "top")
    return(p)
}

#' Timeline plot for \code{\link{trackeRdataSummary}} objects
#'
#' @inheritParams timeline
#' @rdname timeline
#' @export
timeline.trackeRdataSummary <- timeline.trackeRdata


#' @export
"[.trackeRdataSummary" <- function(x, i, j, drop = TRUE, ...) {
    units <- getUnits(x)
    x <- as.data.frame(x)
    ret <- x[i, , drop = drop]

    attr(ret, "units") <- units
    class(ret) <- c("trackeRdataSummary", class(ret))
    return(ret)
}

#' @rdname nsessions
#' @export
nsessions.trackeRdataSummary <- function(object, ...) {
    nrow(object)
}

#' @rdname session_times
#' @export
session_times.trackeRdataSummary <- function(object,
                                             session = NULL,
                                             ...) {
    if (is.null(session)) {
        session <- seq_along(object)
    }
    as.data.frame(object[session])[, c("sessionStart", "sessionEnd")]
}

#' @rdname session_duration
#' @export
session_duration.trackeRdataSummary <- function(object,
                                                session = NULL,
                                                ...) {
    if (is.null(session)) {
        session <- seq_along(object)
    }
    object[session]$duration
}

#' @rdname get_sport
#' @export
get_sport.trackeRdataSummary <- function(object,
                                         session = NULL,
                                         ...) {
    if (is.null(session)) {
        session <- seq.int(nrow(object))
    }
    object[session, ]$sport
}

#' Get the units of the variables in an \code{trackeRdataSummary} object
#'
#' @param object An object of class \code{trackeRdataSummary}.
#' @param ... Currently not used.
#' @export
get_units.trackeRdataSummary <- function(object, ...) {
    attr(object, "units")
}


#' Change the units of the variables in an \code{trackeRdataSummary} object
#'
#' @param object An object of class \code{trackeRdataSummary}.
#' @param variable A vector of variables to be changed. Note, these are expected to be
#'     concepts like 'speed' rather than variable names like 'avgSpeed' or 'avgSpeedMoving'.
#' @param unit A vector with the units, corresponding to variable.
#' @param ... Currently not used.
#' @export
change_units.trackeRdataSummary <- function(object,
                                            variable,
                                            unit,
                                            ...) {

    no_variable <- missing(variable)
    no_unit <- missing(unit)

    if (no_unit & no_variable) {
        return(object)
    }
    else {
        ## NOTE: variable is expected to contain concepts like 'speed' rather than variable
        ## names like 'avgSpeed' or 'avgSpeedMoving'.
        concept <- variable
        units <- get_units(object)
        current <- collect_units(units, unit_reference_sport = attr(object, "unit_reference_sport"))
        p <- length(variable)

        if (length(unit) == p) {
            ## no need for collect_units as this is already done in summary

            mt <- attr(object, "moving_threshold")
            object <- as.data.frame(object)

            for (i in concept) {
                variables <- names(object)[grep(pattern = i, names(object), ignore.case = TRUE)]
                currentUnit <- current$unit[current$variable == i]  ## $concept
                newUnit <- unit[which(concept == i)]
                if (currentUnit != newUnit) {
                    conversion <- match.fun(paste(currentUnit, newUnit, sep = "2"))
                    ## convert summary statistics
                    for (v in variables) {
                        object[, v] <- conversion(object[, v])
                    }
                    ## convert moving threshold
                    if (i == "speed")
                        mt <- conversion(mt)
                    ## update units
                    current$unit[current$variable == i] <- newUnit
                }

            }

            ## update units in units
            for (va in current$variable) {
                units$unit[units$variable == va] <- current$unit[current$variable == va]
            }

            ## update units attribute and return
            attr(object, "units") <- units
            attr(object, "moving_threshold") <- mt
            class(object) <- c("trackeRdataSummary", class(object))
            return(object)
        }
        else {
            stop("variable and unit should have the same length.")
        }
    }
}


