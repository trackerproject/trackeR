#' Summary of training sessions.
#'
#' @param object An object of class \code{\link{trackeRdata}}.
#' @param session A numeric vector of the sessions to be summarised, defaults to all sessions.
#' @param movingThreshold The threshold above which speed an athlete is considered moving (given in the unit of the speed measurements in \code{object}. If \code{NULL}, the default, the threshold corresponds to a slow walking speed (1 m/s, converted to another speed unit, if necessary). For reference, the preferred walking speed for humans is around 1.4 m/s (Bohannon, 1997).
#' @param ... Currently not used.
#' @return An object of class \code{trackeRdataSummary}.
#' @seealso \code{\link{plot.trackeRdataSummary}}
#' @references Bohannon RW (1997). "Comfortable and Maximum Walking Speed of Adults Aged 20--79 Years: Reference Values and Determinants." Age and Ageing, 26(1), 15--19. doi: 10.1093/ageing/26.1.15.
#' @examples
#' data(runs, package = "trackeR")
#' runSummary <- summary(runs, session = 1:2)
#' ## print summary
#' runSummary
#' print(runSummary, digits = 3)
#' ## change units
#' changeUnits(runSummary, variable = "speed", unit = "km_per_h")
#' ## plot summary
#' runSummaryFull <- summary(runs)
#' plot(runSummaryFull)
#' plot(runSummaryFull, group = c("total", "moving"),
#'     what = c("avgSpeed", "distance", "duration", "avgHeartRate"))
#' @export
summary.trackeRdata <- function(object, session = NULL, movingThreshold = NULL, ...){

    ## threshold defining 'moving'
    units <- getUnits(object)
    if (is.null(movingThreshold)){
        ## set to a speed (somewhat) below the preferred walking speed of ~1.4 m/s (Bohannon, 1997)
        movingThreshold <- 1
        speedUnit <- units$unit[units$variable == "speed"]
        if (speedUnit != "m_per_s") {
            conversion <- match.fun(paste("m_per_s", speedUnit, sep = "2"))
            movingThreshold <- conversion(movingThreshold)
        }
    }

    ## select sessions
    if (is.null(session)) session <- 1:length(object)
    object <- object[session]

    ## session times
    sessionStart <- as.POSIXct(sapply(object, function(x) min(index(x))), origin = "1970-01-01")
    sessionEnd <- as.POSIXct(sapply(object, function(x) max(index(x))), origin = "1970-01-01")

    ## distance
    distance <- sapply(object, function(x) zoo::coredata(x$distance)[nrow(x)])

    ## session length (unit set by difftime)
    duration <- difftime(sessionEnd, sessionStart)
    durUnit <- switch(units(duration), "secs" = "s", "mins" = "min", "hours" = "h",
                        "days" = "d") ## README: can be avoided if we use the same names...
    if ("duration" %in% units$variable) {
        units$unit[units$variable == "duration"] <- durUnit
    } else {
        units <- rbind(units, c("duration", durUnit))
    }
    
    ## moving time (based on speed)
    durationMoving <- sapply(object, function(x) timeAboveThreshold(x$speed, threshold = movingThreshold, ge = FALSE))
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
    avgSpeed <- dist4speed / dur4speed

    ## average speed moving
    durMoving4speed <- conversionDur(as.numeric(durationMoving))
    avgSpeedMoving <- dist4speed / durMoving4speed

    ## average pace
    distUnit4pace <- strsplit(units$unit[units$variable == "pace"], split = "_per_")[[1]][2]
    conversionDistPace <- match.fun(paste(distUnit, distUnit4pace, sep = "2"))
    dist4pace <- conversionDistPace(distance)
    avgPace <- as.numeric(duration, units = "mins") / dist4pace

    ## average pace moving
    avgPaceMoving <- as.numeric(durationMoving, units = "mins") / dist4pace

    ## function for weighted total to produce averages
    weightedMean <- function(x, which, th, resting = FALSE){
        z <- coredata(x[, which])[-length(x[, which])]
        if (all(is.na(z))) return(NA)

        dt <- as.numeric(diff(index(x)))
        i <- as.numeric(!is.na(z))
        m <- as.numeric(x$speed[-nrow(x)] > th)
        if (resting) m <- 1 - m

        w <- dt*i*m / sum(dt*i*m)
        ret <- sum(z*w, na.rm = TRUE)
        return(ret)
    }

    ## ## average speed
    ## avgSpeed <- sapply(object, weightedMean, which = "speed", th = -1)

    ## ## average speed moving
    ## avgSpeedMoving <- sapply(object, weightedMean, which = "speed", th = movingThreshold)

    ## ## average pace
    ## avgPace <- sapply(object, weightedMean, which = "pace", th = -1)

    ## ## average pace moving
    ## avgPaceMoving <- sapply(object, weightedMean, which = "pace", th = movingThreshold)

    ## average cadence
    avgCadence <- sapply(object, weightedMean, which = "cadence", th = -1)

    ## average cadence moving
    avgCadenceMoving <- sapply(object, weightedMean, which = "cadence", th = movingThreshold)

    ## average power
    avgPower <- sapply(object, weightedMean, which = "power", th = -1)

    ## average power moving
    avgPowerMoving <- sapply(object, weightedMean, which = "power", th = movingThreshold)

     ## average heart rate
    avgHeartRate <- sapply(object, weightedMean, which = "heart.rate", th = -1)

    ## average heart rate moving
    avgHeartRateMoving <- sapply(object, weightedMean, which = "heart.rate", th = movingThreshold)

    ## average heart rate resting
    avgHeartRateResting <- sapply(object, weightedMean, which = "heart.rate", th = movingThreshold, resting = TRUE)

    ## work to rest ratio
    wrRatio <- as.numeric(durationMoving) / as.numeric(duration - durationMoving)


    ## maxima in addition to averages?
    ## calories?
    ## splits per km?


    ret <- data.frame(session = session, sessionStart = sessionStart, sessionEnd = sessionEnd,
                      distance = distance, duration = duration, durationMoving = durationMoving, 
                      avgSpeed = avgSpeed, avgSpeedMoving = avgSpeedMoving,
                      avgPace = avgPace, avgPaceMoving = avgPaceMoving,
                      avgCadence = avgCadence, avgCadenceMoving = avgCadenceMoving,
                      avgPower = avgPower, avgPowerMoving = avgPowerMoving,
                      avgHeartRate = avgHeartRate, avgHeartRateMoving = avgHeartRateMoving,
                      avgHeartRateResting = avgHeartRateResting, 
                      wrRatio = wrRatio)

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
print.trackeRdataSummary <- function(x, ..., digits = 2){
    units <- getUnits(x)

    for(i in seq_len(length(x$session))){
        cat("\n *** Session", x$session[i], "***\n")

        cat("\n Session times:",
        format(x$sessionStart[i], format = "%Y-%m-%d %H:%M:%S"), "-",
        format(x$sessionEnd[i], format = "%Y-%m-%d %H:%M:%S"), "\n ")

        cat("Distance:",
            round(x$distance[i], digits),
            units$unit[units$variable == "distance"], "\n ")

        cat("Duration:",
            round(as.numeric(x$duration[i]), digits),
            units(x$duration[i]), "\n ")

        cat("Moving time:",
            round(x$durationMoving[i], digits),
            units(x$durationMoving[i]), "\n ")

        cat("Average speed:",
            round(x$avgSpeed[i], digits = digits),
            units$unit[units$variable == "speed"], "\n ")

        cat("Average speed moving:",
            round(x$avgSpeedMoving[i], digits = digits),
            units$unit[units$variable == "speed"], "\n ")

        unitDist4pace <- strsplit(units$unit[units$variable == "pace"],
                                  split = "_per_")[[1]][2]
        avgPace <- floor(x$avgPace[i] * 100) / 100
        cat(paste0("Average pace (per 1 ", unitDist4pace, "):"),
            paste(floor(avgPace), round(avgPace %% 1 * 60, 0), sep = ":"), "min:sec\n ")

        avgPaceMoving <- floor(x$avgPaceMoving[i] * 100) / 100
        cat(paste0("Average pace moving (per 1 ", unitDist4pace, "):"),
            paste(floor(avgPaceMoving) , round(x$avgPaceMoving[i] %% 1 * 60, 0), sep = ":"), "min:sec\n ")

        cat("Average cadence:",
            round(x$avgCadence[i], digits = digits),
            units$unit[units$variable == "cadence"], "\n ")

        cat("Average cadence moving:",
            round(x$avgCadenceMoving[i], digits = digits),
            units$unit[units$variable == "cadence"], "\n ")

        cat("Average power:",
            round(x$avgPower[i], digits = digits),
            units$unit[units$variable == "power"], "\n ")

        cat("Average power moving:",
            round(x$avgPowerMoving[i], digits = digits),
            units$unit[units$variable == "power"], "\n ")

        cat("Average heart rate:",
            round(x$avgHeartRate[i], digits = digits),
            units$unit[units$variable == "heart.rate"], "\n ")

        cat("Average heart rate moving:",
            round(x$avgHeartRateMoving[i], digits = digits),
            units$unit[units$variable == "heart.rate"], "\n ")

        cat("Average heart rate resting:",
            round(x$avgHeartRateResting[i], digits = digits),
            units$unit[units$variable == "heart.rate"], "\n ")

        cat("Work to rest ratio:",
            round(x$wrRatio[i], digits), "\n")

        cat("\n Moving threshold:",
            round(attr(x, "movingThreshold"), digits = digits),
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
fortify.trackeRdataSummary <- function(model, data, melt = FALSE, ...){
    ret <- data.frame(model)

    if(melt){

        basic <- ret[, c("session", "sessionStart", "sessionEnd")]

        varsTotal <- c("distance", "duration", "avgSpeed", "avgPace", "avgCadence", "avgPower",
                       "avgHeartRate", "wrRatio")
        varsMoving <- c("duration", "avgSpeed", "avgPace", "avgCadence", "avgPower", "avgHeartRate")
        varsResting <- c("avgHeartRate")

        dfTotal <- data.frame(basic[rep(ret$session, times = length(varsTotal)),],
                              variable = rep(varsTotal, each = nrow(ret)),
                              value = unlist(ret[, varsTotal]),
                              type = "total")
        dfMoving <- data.frame(basic[rep(ret$session, times = length(varsMoving)),],
                               variable = rep(varsMoving, each = nrow(ret)),
                               value = unlist(ret[, paste0(varsMoving, "Moving")]),
                               type = "moving")
        dfResting <- data.frame(basic[rep(ret$session, times = length(varsResting)),],
                               variable = rep(varsResting, each = nrow(ret)),
                               value = unlist(ret[, paste0(varsResting, "Resting")]),
                               type = "resting")
        
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
#' @param ... Currently not used.
#' @seealso \code{\link{summary.trackeRdata}}
#' @examples
#' data(runs, package = "trackeR")
#' runSummary <- summary(runs)
#' plot(runSummary)
#' plot(runSummary, date = FALSE, group = "total",
#'     what = c("distance", "duration", "avgSpeed"))
#' @export
plot.trackeRdataSummary <- function(x, date = TRUE, what = NULL, group = NULL, ...){
    ## the following line is just intended to prevent R CMD check to produce the NOTE
    ## "no visible binding for global variable *" because those variables are used in subset()
    variable <- type <- NULL

    nsessions <- length(unique(x$session))
    ndates <- length(unique(x$sessionStart))
    units <- getUnits(x)

    ## subsets on variables and type
    dat <- fortify(x, melt = TRUE)
    if (!is.null(what)){
        dat <- subset(dat, variable %in% what)
    }
    if (!is.null(group)){
        dat <- subset(dat, type %in% group)
    }

    ## remove empty factor levels
    dat$variable <- factor(dat$variable)
    #dat$type <- factor(dat$type)

    ## clean up: if there are only NA observations for a variable, the (free) y-scale cannot be determined
    empty <- tapply(dat$value, dat$variable, function(x) all(is.na(x)))
    if (any(empty)) dat <- subset(dat, !(variable %in% names(empty)[empty]))

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
    if (date & ndates < nsessions) stop("All sessions must have unique starting times. Try date = FALSE instead.")
    p <- p + ggplot2::geom_point(ggplot2::aes_(x = quote(xaxis), y = quote(value), color = quote(type))) +
        ggplot2::labs(x = xlab, y = "") +
        ggplot2::guides(color = ggplot2::guide_legend(title = "Type"))
    if (nsessions > 1)
        p <- p + ggplot2::geom_line(ggplot2::aes_(x = quote(xaxis), y = quote(value), color = quote(type))) +
        ggplot2::guides(color = ggplot2::guide_legend(title = "Type"))

    ## facets
    lab_sum <- function(series){
        series <- as.character(series)
        if (series == "wrRatio") return("wrRatio")
        concept <- switch(series, avgPace = "pace", avgSpeed = "speed",
                          distance = "distance", duration = "duration",
                          avgPower = "power", avgCadence = "cadence", avgHeartRate = "heart.rate")
        thisunit <- units$unit[units$variable == concept]
        prettyUnit <- prettifyUnits(thisunit)
        paste0(series, " [", prettyUnit,"]")
    }
    lab_sum <- Vectorize(lab_sum)

    p <- p + ggplot2::facet_grid(facets = "variable ~ .", scales = "free_y",
                                 labeller = ggplot2::labeller("variable" = lab_sum)) +
        ggplot2::theme(legend.position = "top")

    ## add bw theme
    p <- p + ggplot2::theme_bw()

    return(p)
}

#' @export
"[.trackeRdataSummary" <- function(x, i, j, drop = TRUE, ...){
    units <- getUnits(x)
    x <- as.data.frame(x)
    ret <- x[i,, drop = drop]

    attr(ret, "units") <- units
    class(ret) <- c("trackeRdataSummary", class(ret))
    return(ret)
}
