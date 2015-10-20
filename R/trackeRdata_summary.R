#' Summary of training sessions.
#'
#' @param object An object of class \code{\link{trackeRdata}}.
#' @param session A numeric vector of the sessions to be summarised, defaults to all sessions.
#' @param ... Currently not used.
#' @return An object of class \code{trackeRdataSummary}.
#' @seealso \code{\link{plot.trackeRdataSummary}}
#' @examples
#' data(run, package = "trackeR")
#' runSummary <- summary(run)
#' runSummary
#' print(runSummary, digits = 3)
#' plot(runSummary)
#' @export
summary.trackeRdata <- function(object, session = NULL, ...){

    units <- getUnits(object)

    ## select sessions
    if (is.null(session)) session <- 1:length(object)
    object <- object[session]

    ## session times
    sessionStart <- as.POSIXct(sapply(object, function(x) min(index(x))), origin = "1970-01-01")
    sessionEnd <- as.POSIXct(sapply(object, function(x) max(index(x))), origin = "1970-01-01")

    ## session length (unit set by difftime)
    duration <- difftime(sessionEnd, sessionStart)
    durUnit <- switch(units(duration), "secs" = "s", "mins" = "min", "hours" = "h",
                        "days" = "d") ## README: can be avoided if we use the same names...
    units <- rbind(units, c("duration", durUnit))

    ## distance
    distance <- sapply(object, function(x) zoo::coredata(x$distance)[nrow(x)])

    ## average speed (includes time when device was paused / imputed times)
    distUnit <- units$unit[units$variable == "distance"]
    unitSpeed <- strsplit(units$unit[units$variable == "speed"], split = "_per_")[[1]]
    conversionDist <- match.fun(paste(distUnit, unitSpeed[1], sep = "2"))
    dist4speed <- conversionDist(distance)
    conversionDur <- match.fun(paste(durUnit, unitSpeed[2], sep = "2"))
    dur4speed <- conversionDur(as.numeric(duration))
    avgSpeed <- dist4speed / dur4speed

    ## average pace
    ## (in min per 1 km if speed unit refers to km or m,
    ## and in min per 1 mile if speed unit refers to ft or mi)
    if (length(units$unit[units$variable == "pace"]) > 0) {
        distUnit4pace <- strsplit(units$unit[units$variable == "pace"], split = "_per_")[[1]][2]
    } else {
        distUnit4pace <- switch(unitSpeed[1], km = "km", m = "km", ft = "mi", mi = "mi")
    }
    conversionDistPace <- match.fun(paste(distUnit, distUnit4pace, sep = "2"))
    dist4pace <- conversionDistPace(distance)
    avgPace <- as.numeric(duration, units = "mins") / dist4pace
    units <- rbind(units, c("pace", paste("min", distUnit4pace, sep = "_per_")))

    ## moving time (based on speed)
    durationMoving <- sapply(object, function(x) timeAboveThreshold(x$speed, threshold = 0, ge = FALSE))
    attr(durationMoving, "units") <- "secs"
    class(durationMoving) <- "difftime"
    units(durationMoving) <- units(duration)

    ## work to rest ratio
    wrRatio <- as.numeric(durationMoving) / as.numeric(duration - durationMoving)

    ## avg speed moving
    durMoving4speed <- conversionDur(as.numeric(durationMoving))
    avgSpeedMoving <- dist4speed / durMoving4speed

    ## avg pace moving
    avgPaceMoving <- as.numeric(durationMoving, units = "mins") / dist4pace

    ## function for weighted total to produce averages
    weightedTotal <- function(x, which, durUnits, moving = FALSE){
        value <- coredata(x[, which])[-length(x[, which])]
        if (all(is.na(value))) return(NA)
        dt <- diff(index(x))
        units(dt) <- durUnits
        ret <- value * as.numeric(dt) #sum(v * dt/dur, na.rm = TRUE)
        if (moving){
            ret <- ret * (x$speed > 0)[-nrow(x)]
        }
        sum(ret, na.rm = TRUE)
    }

    ## average power (Watts)
    wtPower <- sapply(object, weightedTotal, which = "power", durUnits = units(duration), moving = FALSE)
    avgPower <- wtPower / as.numeric(duration)

    ## average power moving
    wtPowerMoving <- sapply(object, weightedTotal, which = "power", durUnits = units(durationMoving), moving = TRUE)
    avgPowerMoving <- wtPowerMoving / as.numeric(durationMoving)

    ## average cadence
    wtCad <- sapply(object, weightedTotal, which = "cadence", durUnits = units(duration), moving = FALSE)
    avgCadence <- wtCad / as.numeric(duration)

    ## average cadence moving
    wtCadMoving <- sapply(object, weightedTotal, which = "cadence", durUnits = units(durationMoving), moving = TRUE)
    avgCadenceMoving <- wtCadMoving / as.numeric(durationMoving)

    ## ## average heart rate
    ## wtHR <- sapply(object, weightedTotal, which = "heart.rate", durUnits = units(duration), moving = FALSE)
    ## avgHeartRate <- wtHR / as.numeric(duration)
    ## ## average heart rate moving
    ## wtHRMoving <- sapply(object, weightedTotal, which = "heart.rate", durUnits = units(durationMoving),
    ##                      moving = TRUE)
    ## avgHeartRateMoving <- wtHRMoving / as.numeric(durationMoving)

    ## average heart rate
    ## missing values in heart rate cannot implicitly be treated as 0,
    ## (as they are if sum(..., na.rm = TRUE) is divided by a duration which includes NA values in HR
    ## because it's based on speed > 0)
    ## thus duration should capture the time for which heart rate records are available.
    durationHR <- lapply(object, function(x) timeAboveThreshold(x$heart.rate, threshold = 0, ge = FALSE))
    attr(durationHR, "units") <- "secs"
    class(durationHR) <- "difftime"
    wtHR <- sapply(object, weightedTotal, which = "heart.rate", durUnit = units(durationHR), moving = FALSE)
    avgHeartRate <- wtHR / as.numeric(durationHR)
    avgHeartRate <- ifelse(isTRUE(all.equal(avgHeartRate, 0)) | is.na(avgHeartRate), NA, avgHeartRate)


    ## maxima in addition to averages?
    ## calories?
    ## splits per km?


    ret <- data.frame(session = session, sessionStart = sessionStart, sessionEnd = sessionEnd,
                      duration = duration, distance = distance, avgSpeed = avgSpeed, avgPace = avgPace,
                      avgPower = avgPower, avgCadence = avgCadence, avgHeartRate = avgHeartRate,
                      durationMoving = durationMoving, wrRatio = wrRatio, avgSpeedMoving = avgSpeedMoving,
                      avgPaceMoving = avgPaceMoving, avgPowerMoving = avgPowerMoving,
                      avgCadenceMoving = avgCadenceMoving) #, avgHeartRateMoving = avgHeartRateMoving)
    attr(ret, "units") <- units
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

        cat("\n Session times:\n",
        format(x$sessionStart[i], format = "%Y-%m-%d %H:%M:%S"), "-",
        format(x$sessionEnd[i], format = "%Y-%m-%d %H:%M:%S"), "\n")

        cat("\n Distance:\n",
            round(x$distance[i], digits), units$unit[units$variable == "distance"], "\n")

        ## averages over total
        cat("\n Duration:\n",
            round(as.numeric(x$duration[i]), digits), units(x$duration[i]), "\n")

        cat("\n Average speed:\n",
            round(x$avgSpeed[i], digits = digits), units$unit[units$variable == "speed"], "\n")

        unitDist4pace <- strsplit(units$unit[units$variable == "pace"], split = "_per_")[[1]][2]
        avgPace <- floor(x$avgPace[i] * 100) / 100
        cat("\n", paste0("Average pace (per 1 ", unitDist4pace, "):"), "\n",
            paste(floor(avgPace) , round(avgPace %% 1 * 60, 0), sep = ":"), "min:sec\n")

        cat("\n Average heart rate:\n",
            round(x$avgHeartRate[i], digits = digits), units$unit[units$variable == "heart.rate"], "\n")

        cat("\n Average cadence:\n",
            round(x$avgCadence[i], digits = digits), units$unit[units$variable == "cadence"], "\n")

        cat("\n Average power:\n",
            round(x$avgPower[i], digits = digits), units$unit[units$variable == "power"], "\n")

        ## averages over moving
        cat("\n Moving time:\n",
            round(x$durationMoving[i], digits), units(x$durationMoving[i]), "\n")

        cat("\n Work to rest ratio:\n",
            round(x$wrRatio[i], digits), "\n")

        cat("\n Average speed moving:\n",
            round(x$avgSpeedMoving[i], digits = digits), units$unit[units$variable == "speed"], "\n")

        avgPaceMoving <- floor(x$avgPaceMoving[i] * 100) / 100
        cat("\n", paste0("Average pace moving (per 1 ", unitDist4pace, "):"), "\n",
            paste(floor(avgPaceMoving) , round(x$avgPaceMoving[i] %% 1 * 60, 0), sep = ":"), "min:sec\n")

        ## cat("\n Average heart rate moving:\n",
        ##     round(x$avgHeartRateMoving[i], digits = digits), units$unit[units$variable == "heart.rate"], "\n")

        cat("\n Average cadence moving:\n",
            round(x$avgCadenceMoving[i], digits = digits), units$unit[units$variable == "cadence"], "\n")

        cat("\n Average power moving:\n",
            round(x$avgPowerMoving[i], digits = digits), units$unit[units$variable == "power"], "\n")

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

        varsTotal <- c("duration", "distance", "avgSpeed", "avgPace", "avgPower", "avgCadence", "avgHeartRate", "wrRatio")
        varsMoving <- c("duration", "avgSpeed", "avgPace")

        dfTotal <- data.frame(basic[rep(ret$session, times = length(varsTotal)),],
                              variable = rep(varsTotal, each = nrow(ret)),
                              value = unlist(ret[, varsTotal]),
                              type = "total")
        dfMoving <- data.frame(basic[rep(ret$session, times = length(varsMoving)),],
                            variable = rep(varsMoving, each = nrow(ret)),
                            value = unlist(ret[, paste0(varsMoving, "Moving")]),
                            type = "moving")
        ret <- rbind(dfTotal, dfMoving)
    }
    return(ret)
}


#' Plot an object of class trackeRdataSummary.
#'
#' @param x An object of class \code{trackeRdataSummary}.
#' @param xvar Should the date or the session number be used on the abscissa? Default is date.
#' @param what Name of variables which should be plotted. Default is all.
#' @param group Which group of variables should be plotted? This can either be
#'     \code{total} or \code{moving}. Default is both.
#' @param ... Currently not used.
#' @seealso \code{\link{summary.trackeRdata}}
#' @examples
#' data(run, package = "trackeR")
#' runSummary <- summary(run)
#' plot(runSummary)
#' plot(runSummary, xvar = "session", what = c("distance", "duration", "avgSpeed"), group = "total")
#' @export
plot.trackeRdataSummary <- function(x, xvar = c("date", "session"), what = NULL, group = NULL, ...){

    nsessions <- length(unique(x$session))
    ndates <- length(unique(x$sessionStart))
    units <- getUnits(x)

    xvar <- match.arg(tolower(xvar), c("date", "session"))
    date <- xvar == "date"

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
    p <- p + ggplot2::geom_point(ggplot2::aes(x = xaxis, y = value, color = type)) +
            ggplot2::labs(x = xlab, y = "")
    if (nsessions > 1)
        p <- p + ggplot2::geom_line(ggplot2::aes(x = xaxis, y = value, color = type))

    ## facets
    lab <- function(variable, value){
        if (variable == "variable"){
            ## match variable with concept
            value <- as.character(value)
            concept <- switch(value, avgPace = "pace", avgSpeed = "speed",
                              distance = "distance", duration = "duration",
                              avgPower = "power", avgCadence = "cadence", avgHeartRate = "heart.rate")
            ret <- paste0(value, " [", units$unit[units$variable == concept], "]")
            if (value == "wrRatio") ret <- "wrRatio"
        } else {
            ret <- as.character(value)
        }
        return(ret)
    }
    lab <- Vectorize(lab)
    p <- p + ggplot2::facet_grid(facets = variable ~ ., scales = "free_y", labeller = lab) +
        ggplot2::theme(legend.position = "top")

    ## add bw theme
    p <- p + ggplot2::theme_bw()

    return(p)
}
