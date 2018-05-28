## FIXME: export? write more flexible so that also univariate zoos can be imputed? Do this via a generic?

#' Impute speeds.
#'
#' Impute speeds of 0 during small breaks within a session.
#'
#' @param sessionData A multivariate \code{\link[zoo]{zoo}} object with
#'     observations of either distance or speed (named Distance or Speed,
#'     respectively).
#' @param fromDistances Logical. Should the speeds be calculated from the distance recordings
#'     instead of taken from the speed recordings directly?
#' @param lgap Time in seconds corresponding to the minimal sampling rate.
#' @param lskip Time in seconds between the last observation before a small break
#'     and the first imputed speed or the last imputed speed and the first
#'     observation after a small break.
#' @param m Number of imputed observations in each small break.
#' @param cycling Logical. Are the data from a cycling session? If \code{TRUE}, power is
#'     imputed with \code{0}, else with \code{NA}.
#' @param units Units of measurement.
#'
#' @return A multivariate \code{\link[zoo]{zoo}} object with imputed observations:
#'     0 for speed, last known position for latitude, longitude and altitude,
#'     NA for all other variables. Distances are calculated based on speeds after imputation.
#' @references Kosmidis, I., and Passfield, L. (2015). Linking the Performance of
#'     Endurance Runners to Training and Physiological Effects via Multi-Resolution
#'     Elastic Net. \emph{ArXiv e-print} arXiv:1506.01388.
#'     Frick, H., Kosmidis, I. (2017). trackeR: Infrastructure for Running and Cycling Data from GPS-Enabled Tracking Devices in R. \emph{Journal of Statistical Software}, \bold{82}(7), 1--29. doi:10.18637/jss.v082.i07
imputeSpeeds <- function(sessionData, fromDistances = TRUE, lgap = 30, lskip = 5, m = 11,
                         cycling = FALSE, units = NULL) {

    if (length(sessionData) < 2) {
        return(sessionData)
    }

    if (is.null(units)) units <- generateBaseUnits(cycling)
    distUnit <- units$unit[units$variable == "distance"]
    speedUnits <- strsplit(units$unit[units$variable == "speed"], "_per_")[[1]]
    distUnitSpeed <- speedUnits[1]
    timeUnitSpeed <- switch(speedUnits[2], "s" = "secs", "min" = "mins", "h" = "hours", "d" = "days") ## README: can be avoided if we use the same names...

    ## Calculate speeds
    if (fromDistances){
        if (all(is.na(sessionData$distance))) {
            warning("No distances are available to calculate the speeds. If available, measurements of speed are used instead.")
            ## check if speed data is available as an alternative, otherwise return sessionData
            if (all(is.na(sessionData$speed))) {
                return(sessionData)
            }
        } else {
            sessionData <- sessionData[!is.na(sessionData$distance)]
            if (distUnit != distUnitSpeed){
                conversion <- match.fun(paste(distUnit, distUnitSpeed, sep = "2"))

                dist <- conversion(coredata(sessionData$distance))
            } else {
                dist <- coredata(sessionData$distance)
            }
            sessionData$speed <- distance2speed(dist, index(sessionData), timeunit = timeUnitSpeed)
        }
    } else {
        if (all(is.na(sessionData$speed))) {
            warning("No speeds are available. If available, distances are used to calculate speed.")
            if (!all(is.na(sessionData$distance))) {
                sessionData <- sessionData[!is.na(sessionData$distance)]
                if (distUnit != distUnitSpeed){
                    conversion <- match.fun(paste(distUnit, distUnitSpeed, sep = "2"))
                    dist <- conversion(coredata(sessionData$distance))
                } else {
                    dist <- coredata(sessionData$distance)
                }
                sessionData$speed <- distance2speed(dist, index(sessionData), timeunit = timeUnitSpeed)
            } else {
                return(sessionData)
            }
        }
    }

    ## order variables for imputation:
    ## variables with 'content' imputation and variables with NA imputation
    originalOrder <- names(sessionData)
    if (cycling){
        impC <- match(c("latitude", "longitude", "altitude", "distance", "speed", "power"), names(sessionData))
        impN <- which(is.na(match(names(sessionData), c("latitude", "longitude", "altitude", "distance", "speed", "power"))))
        impPower <- 0
        nN <- length(impN)
    }
    else {
        impC <- match(c("latitude", "longitude", "altitude", "distance", "speed"), names(sessionData))
        impN <- which(is.na(match(names(sessionData), c("latitude", "longitude", "altitude", "distance", "speed"))))
        impPower <- NA
        nN <- length(impN) - 1
    }
    sessionData <- sessionData[, c(impC, impN)]

    ## Remove observations with negative or missing speeds
    sessionData <- sessionData[sessionData$speed >= 0 & !is.na(sessionData$speed)]

    ## get session parts (which are separated by short breaks lasting more than lgap seconds)
    shortBreaks <- restingPeriods(index(sessionData), lgap/3600)

    ## Put some zeros within the short breaks
    #nObs <- nrow(sessionData)
    nOther <- ncol(sessionData) - 1
    nLaps <- nrow(shortBreaks$sessions)
    ## if there are more than 1 laps then impute zero speeds
    imputedData <- zoo(x = matrix(NA, nrow = 0, ncol = ncol(sessionData),
                           dimnames = list(NULL, names(sessionData))), order.by = as.POSIXct("1970-01-01")[c()])
    if (nLaps > 1) {
        for (j in seq.int(nLaps)[-nLaps]) {
            newtimes <- with(shortBreaks$sessions,
                             seq(sessionEnd[j] + lskip,
                                 sessionStart[j + 1] - lskip,
                                 length.out = m))
            newdata <- matrix(c(
                ## last know position
                as.vector(sessionData[shortBreaks$sessions$sessionEnd[j], c("latitude", "longitude", "altitude")]),
                ## distance (will be updated based on imputed speeds)
                0,
                ## speed
                0,
                ## power
                impPower,
                ## anything else
                rep(NA, nN)), ncol = ncol(sessionData),
                              dimnames = list(NULL, names(sessionData)))
            imputedData <- c(imputedData,
                             zoo(x = newdata, order.by = newtimes))
        }
    }
    ## Add observations at the begininng and end
    newtimesStart <- seq(shortBreaks$sessions$sessionStart[1] - 5,
                         shortBreaks$sessions$sessionStart[1] - 1,
                         length = m)
    newdataStart <- matrix(c(
        ## first know position
        as.vector(sessionData[shortBreaks$sessions$sessionStart[1], c("latitude", "longitude", "altitude")]),
        ## distance (will be updated based on imputed speeds)
        0,
        ## speed
        0,
        ## power
        impPower,
        ## anything else
        rep(NA, nN)), ncol = ncol(sessionData),
                           dimnames = list(NULL, names(sessionData)))
    newtimesEnd <- seq(shortBreaks$sessions$sessionEnd[nLaps] + 1,
                       shortBreaks$sessions$sessionEnd[nLaps] + 5,
                       length = m)
    newdataEnd <- matrix(c(
        ## last know position
        as.vector(sessionData[shortBreaks$sessions$sessionEnd[nLaps], c("latitude", "longitude", "altitude")]),
        ## distance (will be updated based on imputed speeds)
        0,
        ## speed
        0,
        ## power
        impPower,
        ## anything else
        rep(NA, nN)), ncol = ncol(sessionData),
                         dimnames = list(NULL, names(sessionData)))
    imputedData <- c(imputedData,
                     zoo(x = newdataStart, order.by = newtimesStart),
                     zoo(x = newdataEnd, order.by = newtimesEnd))
    sessionData <- c(sessionData, imputedData)


    ## update distances
    updatedDistance <- speed2distance(sessionData$speed, index(sessionData), timeunit = timeUnitSpeed)
    if (distUnit != distUnitSpeed) {
        conversion <- match.fun(paste(distUnitSpeed, distUnit, sep = "2"))
        updatedDistance <- conversion(updatedDistance)
    }
    sessionData$distance <- zoo(updatedDistance, order.by = index(sessionData)) ## cumsum doesn't return a zoo object


    ## clean up and return
    sessionData <- sessionData[, originalOrder]
    rownames(sessionData) <- NULL
    return(sessionData)
}

#' Convert distance to speed.
#'
#' @param distance Distance in meters.
#' @param time Time.
#' @param timeunit Time unit in speed, e.g., "hours" for speed in *_per_h.
#' @return Speed in meters per second.
distance2speed <- function(distance, time, timeunit){
    speed <- c(diff(distance) / unclass(difftime(time[-1], time[-length(time)], units = timeunit)), 0)
    ## README: doesn't work if pervious distance is NA, needs to be impute with last known distance.
    return(speed)
}

#' Convert speed to distance.
#'
#' @param speed Speed in meters per second.
#' @param time Time.
#' @param timeunit Time unit in speed, e.g., "hours" for speed in *_per_h.
#' @param cumulative Logical. Should the cumulative distances be returned?
#' @return Distance in meters.
speed2distance <- function(speed, time, timeunit, cumulative = TRUE){
    distance <- c(0, speed[-length(speed)] * unclass(difftime(time[-1], time[-length(time)], units = timeunit)))
    if (cumulative) distance <- cumsum(distance)  ## README: cumsum can't handle NAs
    return(distance)
}



