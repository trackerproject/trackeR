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
#'
#' @return A multivariate \code{\link[zoo]{zoo}} object with imputed observations:
#'     0 for speed, last known position for latitude, longitude and altitude,
#'     NA for all other variables. Distances are calculated based on speeds after imputation.
#' @references Kosmidis, I., and Passfield, L. (2015). Linking the Performance of
#'     Endurance Runners to Training and Physiological Effects via Multi-Resolution
#'     Elastic Net. \emph{ArXiv e-print} arXiv:1506.01388.
imputeSpeeds <- function(sessionData, fromDistances = TRUE, lgap = 30, lskip = 5, m = 11,
                         cycling = FALSE) {

    if (length(sessionData) < 2) {
        return(sessionData)
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

    ## Calculate speeds
    if (fromDistances){
        if (all(is.na(sessionData$distance))) {
            warning("No distances are available to calculate the speeds.")
            return(sessionData)
        } else {
            sessionData <- sessionData[!is.na(sessionData$distance)]
            ##sessionData$speed <- c(diff(sessionData$distance)/unclass(diff(index(sessionData))), NA)
            sessionData$speed <- distance2speed(coredata(sessionData$distance), index(sessionData))
        }
    }
    else {
        if (all(is.na(sessionData$speed))) {
            warning("No speeds are available.")
            return(sessionData)
        }
    }

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
    sessionData$distance <- zoo(speed2distance(sessionData$speed, index(sessionData)),
                                order.by = index(sessionData)) ## cumsum doesn't return a zoo object


    ## clean up and return
    sessionData <- sessionData[, originalOrder]
    rownames(sessionData) <- NULL
    return(sessionData)
}

#' Convert distance to speed.
#'
#' @param distance Distance in meters.
#' @param time Time.
#' @return Speed in meters per second.
distance2speed <- function(distance, time){
    speed <- c(diff(distance) / unclass(diff(time, units = "secs")), 0)
    ## README: doesn't work if pervious distance is NA, needs to be impute with last known distance.
    return(speed)
}

#' Convert speed to distance.
#'
#' @param speed Speed in meters per second.
#' @param time Time.
#' @param cumulative Logical. Should the cumulative distances be returned?
#' @return Distance in meters.
speed2distance <- function(speed, time, cumulative = TRUE){
    distance <- c(0, speed[-length(speed)] * unclass(diff(time, units = "secs")))
    if (cumulative) distance <- cumsum(distance)  ## README: cumsum can't handle NAs
    return(distance)
}



