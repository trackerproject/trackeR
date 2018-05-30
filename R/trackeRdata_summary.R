#' Summary of training sessions.
#'
#' @param object An object of class \code{\link{trackeRdata}}.
#' @param session A numeric vector of the sessions to be summarised, defaults to all sessions.
#' @param movingThreshold The threshold above which speed an athlete is considered moving (given in the unit of the speed measurements in \code{object}. If \code{NULL}, the default, the threshold corresponds to a slow walking speed (1 m/s, converted to another speed unit, if necessary). For reference, the preferred walking speed for humans is around 1.4 m/s (Bohannon, 1997).
#' @param ... Currently not used.
#' @return An object of class \code{trackeRdataSummary}.
#' @seealso \code{\link{plot.trackeRdataSummary}}
#' @references Bohannon RW (1997). 'Comfortable and Maximum Walking Speed of Adults Aged 20--79 Years: Reference Values and Determinants.' Age and Ageing, 26(1), 15--19. doi: 10.1093/ageing/26.1.15.
#' @examples
#' data('runs', package = 'trackeR')
#' runSummary <- summary(runs, session = 1:2)
#' ## print summary
#' runSummary
#' print(runSummary, digits = 3)
#' ## change units
#' changeUnits(runSummary, variable = 'speed', unit = 'km_per_h')
#' ## plot summary
#' runSummaryFull <- summary(runs)
#' plot(runSummaryFull)
#' plot(runSummaryFull, group = c('total', 'moving'),
#'     what = c('avgSpeed', 'distance', 'duration', 'avgHeartRate'))
#' @export
summary.trackeRdata <- function(object, session = NULL, movingThreshold = NULL, ...) {

    ## threshold defining 'moving'
    units <- getUnits(object)
    sports <- sport(object)
    files <- attr(object, "file")
    if (is.null(movingThreshold)) {
        ## set to a speed (somewhat) below the preferred walking speed of ~1.4 m/s (Bohannon,
        ## 1997)
        movingThreshold <- 1
        speedUnit <- units$unit[units$variable == "speed"]
        if (speedUnit != "m_per_s") {
            conversion <- match.fun(paste("m_per_s", speedUnit, sep = "2"))
            movingThreshold <- conversion(movingThreshold)
        }
    }

    ## select sessions
    if (is.null(session))
        session <- 1:length(object)

    object <- object[session]

    ## session times
    sessionStart <- as.POSIXct(sapply(object, function(x) min(index(x))), origin = "1970-01-01")
    sessionEnd <- as.POSIXct(sapply(object, function(x) max(index(x))), origin = "1970-01-01")

    ## distance
    #distance <- sapply(object, function(x) zoo::coredata(x$distance)[nrow(x)])
    distance <- sapply(object, function(x) max(zoo::coredata(x$distance), na.rm = TRUE))

    ## session length (unit set by difftime)
    duration <- difftime(sessionEnd, sessionStart)
    durUnit <- switch(units(duration), secs = "s", mins = "min", hours = "h", days = "d")  ## README: can be avoided if we use the same names...
    if ("duration" %in% units$variable) {
        units$unit[units$variable == "duration"] <- durUnit
    } else {
        units <- rbind(units, c("duration", durUnit))
    }

    ## moving time (based on speed)
    durationMoving <- sapply(object, function(x) timeAboveThreshold(x$speed, threshold = movingThreshold,
        ge = FALSE))
    attr(durationMoving, "units") <- "secs"
    class(durationMoving) <- "difftime"
    units(durationMoving) <- units(duration)

    ## average speed
    distUnit <- units$unit[units$variable == "distance"]
    unitSpeed <- strsplit(units$unit[units$variable == "speed"], split = "_per_")[[1]]
    conversionDist <- match.fun(paste(distUnit, unitSpeed[1], sep = "2"))
    dist4speed <- conversionDist(distance)
    conversionDur <- match.fun(paste(durUnit, unitSpeed[2], sep = "2"))
    dur4speed <- conversionDur(as.numeric(duration))
    avgSpeed <- dist4speed/dur4speed

    ## average speed moving
    durMoving4speed <- conversionDur(as.numeric(durationMoving))
    avgSpeedMoving <- dist4speed/durMoving4speed

    ## average pace
    distUnit4pace <- strsplit(units$unit[units$variable == "pace"], split = "_per_")[[1]][2]
    conversionDistPace <- match.fun(paste(distUnit, distUnit4pace, sep = "2"))
    dist4pace <- conversionDistPace(distance)
    avgPace <- as.numeric(duration, units = "mins")/dist4pace

    ## average pace moving
    avgPaceMoving <- as.numeric(durationMoving, units = "mins")/dist4pace

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

    summaries <- sapply(object, weightedMeans, which = c("cadence", "power", "heart.rate", "altitude"), th = movingThreshold)
    ## work to rest ratio
    wrRatio <- as.numeric(durationMoving)/as.numeric(duration - durationMoving)

    ## maxima in addition to averages?  calories?  splits per km?

    ret <- data.frame(session = session, sessionStart = sessionStart, sessionEnd = sessionEnd,
        distance = distance, duration = duration, durationMoving = durationMoving, avgSpeed = avgSpeed,
        avgSpeedMoving = avgSpeedMoving, avgPace = avgPace, avgPaceMoving = avgPaceMoving,
        avgCadence = summaries["cadence", ],
        avgAltitude = summaries["altitude", ],
        avgAltitudeMoving = summaries["altitude_moving", ],
        avgCadenceMoving = summaries["cadence_moving", ],
        avgPower = summaries["power", ],
        avgPowerMoving = summaries["power_moving", ],
        avgHeartRate = summaries["heart.rate", ],
        avgHeartRateMoving = summaries["heart.rate_moving", ],
        avgHeartRateResting = summaries["heart.rate_resting", ],
        wrRatio = wrRatio,
        sport = sports,
        file = files)

    attr(ret, "units") <- units
    attr(ret, "movingThreshold") <- movingThreshold
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
    units <- getUnits(x)

    for (i in seq_len(length(x$session))) {
        cat("\n *** Session", x$session[i], "***\n")

        cat("\n Session times:", format(x$sessionStart[i], format = "%Y-%m-%d %H:%M:%S"),
            "-", format(x$sessionEnd[i], format = "%Y-%m-%d %H:%M:%S"), "\n ")

        cat("Distance:", round(x$distance[i], digits), units$unit[units$variable == "distance"],
            "\n ")

        cat("Duration:", round(as.numeric(x$duration[i]), digits), units(x$duration[i]),
            "\n ")

        cat("Moving time:", round(x$durationMoving[i], digits), units(x$durationMoving[i]),
            "\n ")

        cat("Average speed:", round(x$avgSpeed[i], digits = digits), units$unit[units$variable ==
            "speed"], "\n ")

        cat("Average speed moving:", round(x$avgSpeedMoving[i], digits = digits), units$unit[units$variable ==
            "speed"], "\n ")

        unitDist4pace <- strsplit(units$unit[units$variable == "pace"], split = "_per_")[[1]][2]
        avgPace <- floor(x$avgPace[i] * 100)/100
        cat(paste0("Average pace (per 1 ", unitDist4pace, "):"), paste(floor(avgPace),
            round(avgPace%%1 * 60, 0), sep = ":"), "min:sec\n ")

        avgPaceMoving <- floor(x$avgPaceMoving[i] * 100)/100
        cat(paste0("Average pace moving (per 1 ", unitDist4pace, "):"), paste(floor(avgPaceMoving),
            round(x$avgPaceMoving[i]%%1 * 60, 0), sep = ":"), "min:sec\n ")

        cat("Average cadence:", round(x$avgCadence[i], digits = digits), units$unit[units$variable ==
            "cadence"], "\n ")

        cat("Average cadence moving:", round(x$avgCadenceMoving[i], digits = digits), units$unit[units$variable ==
            "cadence"], "\n ")

        cat("Average power:", round(x$avgPower[i], digits = digits), units$unit[units$variable ==
            "power"], "\n ")

        cat("Average power moving:", round(x$avgPowerMoving[i], digits = digits), units$unit[units$variable ==
            "power"], "\n ")

        cat("Average heart rate:", round(x$avgHeartRate[i], digits = digits), units$unit[units$variable ==
            "heart.rate"], "\n ")

        cat("Average heart rate moving:", round(x$avgHeartRateMoving[i], digits = digits),
            units$unit[units$variable == "heart.rate"], "\n ")

        cat("Average heart rate resting:", round(x$avgHeartRateResting[i], digits = digits),
            units$unit[units$variable == "heart.rate"], "\n ")

        cat("Work to rest ratio:", round(x$wrRatio[i], digits), "\n")

        cat("\n Moving threshold:", round(attr(x, "movingThreshold"), digits = digits),
            units$unit[units$variable == "speed"], "\n")

        cat("\n")
    }
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

        varsTotal <- c("distance", "duration", "avgSpeed", "avgPace", "avgCadence", "avgPower",
            "avgHeartRate", "wrRatio")
        varsMoving <- c("duration", "avgSpeed", "avgPace", "avgCadence", "avgPower", "avgHeartRate")
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


#' Plot an object of class trackeRdataSummary.
#'
#' @param x An object of class \code{trackeRdataSummary}.
#' @param date Should the date or the session number be used on the abscissa?
#' @param what Name of variables which should be plotted. Default is all.
#' @param group Which group of variables should be plotted? This can either be
#'     \code{total} or \code{moving}. Default is both.
#' @param lines Should interpolating lines be plotted?
#' @param ... Currently not used.
#' @seealso \code{\link{summary.trackeRdata}}
#' @examples
#' data('runs', package = 'trackeR')
#' runSummary <- summary(runs)
#' plot(runSummary)
#' plot(runSummary, date = FALSE, group = 'total',
#'     what = c('distance', 'duration', 'avgSpeed'))
#' @export
plot.trackeRdataSummary <- function(x, date = TRUE, what = NULL, group = NULL, lines = TRUE,
    ...) {
    ## the following line is just intended to prevent R CMD check to produce the NOTE 'no
    ## visible binding for global variable *' because those variables are used in subset()
    variable <- type <- NULL

    nsessions <- length(unique(x$session))
    ndates <- length(unique(x$sessionStart))
    units <- getUnits(x)

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
    } else {
        dat$xaxis <- dat$session
        xlab <- "Session"
    }

    ## (basic) plot
    p <- ggplot2::ggplot(dat)
    if (date & ndates < nsessions)
        stop("All sessions must have unique starting times. Try date = FALSE instead.")
    p <- p + ggplot2::geom_point(ggplot2::aes_(x = quote(xaxis), y = quote(value), color = quote(type)),
        na.rm = TRUE) + ggplot2::labs(x = xlab, y = "") + ggplot2::guides(color = ggplot2::guide_legend(title = "Type")) +
        ggplot2::scale_colour_manual(values = c(total = "#76BD58", moving = "#F68BA2",
            resting = "#5EB3F0"))
    ## color palette comes from colorspace::rainbow_hcl(3, c = 70)[c(2,1,3)] [1] '#5EB3F0'
    ## '#F68BA2' '#76BD58' an alternative from
    ## http://colorbrewer2.org/#type=qualitative&scheme=Dark2&n=3
    ## ggplot2::scale_colour_manual(values = c('total' = '#1b9e77', 'moving' = '#d95f02',
    ## 'resting' = '#7570b3'))

    ## possibly add lines for 2 or more sessions
    if (nsessions > 1) {
        if (lines) {
            p <- p + ggplot2::geom_line(ggplot2::aes_(x = quote(xaxis), y = quote(value),
                color = quote(type)), na.rm = TRUE)
        }
    }

    ## facets
    lab_sum <- function(series) {
        series <- as.character(series)
        concept <- switch(series, avgPace = "pace", avgSpeed = "speed", distance = "distance",
            duration = "duration", avgPower = "power", avgCadence = "cadence", avgHeartRate = "heart.rate")
        thisunit <- units$unit[units$variable == concept]
        prettyUnit <- prettifyUnits(thisunit)
        ret <- switch(series, distance = paste0("distance \n [", prettyUnit, "]"), duration = paste0("duration \n [",
            prettyUnit, "]"), avgSpeed = paste0("avg. speed \n [", prettyUnit, "]"), avgPace = paste0("avg. pace \n [",
            prettyUnit, "]"), avgCadence = paste0("avg. cadence \n [", prettyUnit, "]"),
            avgPower = paste0("avg. power \n [", prettyUnit, "]"), avgHeartRate = paste0("avg. heart rate \n [",
                prettyUnit, "]"), wrRatio = "work-to-rest \n ratio")
        ret
    }
    lab_sum <- Vectorize(lab_sum)

    p <- p + ggplot2::facet_grid(facets = "variable ~ .", scales = "free_y", labeller = ggplot2::labeller(variable = lab_sum))  ## +

    ## add bw theme and position of legend
    p <- p + ggplot2::theme_bw() + ggplot2::theme(legend.position = "top")

    return(p)
}

#' @export
timeline.trackeRdataSummary <- function(object, lims = NULL, ...) {
    startdates <- as.Date(object$sessionStart)
    enddates <- as.Date(object$sessionEnd)
    ## Hack to extract times
    endtimes <- object$sessionEnd
    starttimes <- object$sessionStart
    endtimes <- as.POSIXct(as.numeric(difftime(endtimes, trunc(endtimes, "days"), units = "secs")),
        origin = Sys.Date())
    starttimes <- as.POSIXct(as.numeric(difftime(starttimes, trunc(starttimes, "days"),
        units = "secs")), origin = Sys.Date())
    df <- data.frame(sday = startdates, eday = enddates, start = starttimes, end = endtimes)
    if (!is.null(lims)) {
        lims <- as.POSIXct(paste(Sys.Date(), lims))
    }
    p <- ggplot2::ggplot(df) + ## geom_point(aes(x = start, y = sday), alpha = 0.5) + geom_point(aes(x = end, y =
    ## eday), alpha = 0.5) +
    ggplot2::geom_segment(ggplot2::aes_(x = quote(start), xend = quote(end), y = quote(sday),
        yend = quote(eday)), color = '#428bca', size=1)
    ## take care of breaks, limits on the time axes and style of breakpoints
    p <- p + ggplot2::scale_x_datetime(date_labels = "%H:%m", date_breaks = "4 hour", limits = lims)
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 50, hjust = 1)) +
        ggplot2::xlab("Time") + ggplot2::ylab("Date")
    p + ggplot2::theme_bw()
}


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


#' @rdname sport
#' @export
sport.trackeRdataSummary <- function(object, ...) {
    object$sport
}


#' @rdname session_times
#' @export
session_times.trackeRdataSummary <- function(object, ...) {
    as.data.frame(object)[c("sessionStart", "sessionEnd")]
}

#' @rdname session_duration
#' @export
session_duration.trackeRdataSummary <- function(object, ...) {
    object$duration
}
