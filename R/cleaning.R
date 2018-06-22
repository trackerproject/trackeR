#' Sanity checks for tracking data.
#'
#' Heart rate measurements of 0 are set to NA, assuming the athlete is alive.
#' Observations with missing or duplicated time stamps are removed.
#'
#' @param dat Data set to be cleaned up.
#' @param silent Logical. Should warnings be generated if any of the
#'     sanity checks on the data are triggered?
sanity_checks <- function(dat, silent) {
    ## replace heart rate 0 with NA
    hr0 <- dat$heart_rate == 0
    if (any(hr0, na.rm = TRUE)) {
        if (!silent)
            warning("Heart rate measurements of 0 are set to NA.")
        dat$heart_rate[hr0] <- NA
    }

    ## handle NAs
    natime <- is.na(dat$time)
    if (all(natime)) {
        stop("The are no useable timestamps.")
    }
    if (any(natime)) {
        if (!silent)
            warning("Observations with missing time stamps have been removed.")
        dat <- dat[!natime, ]
    }

    ## handle missing data
    nadat <- is.na(dat[, -which(names(dat) == "time")])
    if (all(nadat)) {
        stop("The is no useable data.")
    }

    ## remove duplicates
    duptime <- duplicated(dat$time)
    if (any(duptime)) {
        if (!silent)
            warning("Observations with duplicated time stamps have been removed.")
        dat <- dat[!duptime, ]
    }

    ## order according to time
    dat <- dat[order(dat$time), ]

    rownames(dat) <- NULL
    return(dat)
}

get_altitude <- function(object, country = NULL, mask = TRUE, ...) {
    ## are any locations available?
    firstLoc <- min(which(apply(object[, c("longitude", "latitude")], 1, function(x) !any(is.na(x)))))
    if (!is.finite(firstLoc)) {
        stop("No location data available.")
    }
    ## get ISO country code
    if (is.null(country)) {
        country <- ll2iso(lon = as.numeric(object$longitude[firstLoc]), lat = as.numeric(object$latitude[firstLoc]))
    }
    ## try to download altitude data
    rast <- try(raster::getData("alt", country = country, download = TRUE, mask = mask))
    ## From documentation: 'In the case of alt you can set 'mask' to FALSE. If it is TRUE
    ## values for neighbouring countries are set to NA.'
    if (!inherits(rast, "try-error")) {
        positionData <- data.frame(lng = object$longitude, lat = object$latitude)
        altitude <- raster::extract(rast, positionData, method = "bilinear")
    }
    else {
        stop("Altitude data could not be downloaded.")
    }
    return(as.numeric(altitude))
}

ll2iso <- function(lon, lat) {
    country <- as.character(ggmap::revgeocode(c(lon, lat), output = "more")$country)
    ref <- data.frame(raster::getData("ISO3"), stringsAsFactors = FALSE)
    isocode <- ref$ISO3[ref$NAME == country]
    return(isocode)
}

distance_correction <- function(object, country = NULL, mask = TRUE, ...) {
    ## get altitude data
    altitudeDwl <- try(get_altitude(object, country = country, mask = mask))
    if (!inherits(altitudeDwl, "try-error")) {
        object$altitude <- altitudeDwl
    }
    ## correct GPS distances if altitude information available
    if (!all(is.na(object$altitude))) {
        object$distance <- cumsum(c(sqrt(diff(object$distance)^2 + diff(object$altitude)^2),
            0))
    }
    else {
        warning("No altitude information is available. Distances are not corrected for elevation.")
    }
    return(object)
}


#' Impute speeds.
#'
#' Impute speeds of 0 during small breaks within a session.
#'
#' @param session_data A multivariate \code{\link[zoo]{zoo}} object with
#'     observations of either distance or speed (named Distance or Speed,
#'     respectively).
#' @param from_distances Logical. Should the speeds be calculated from the distance recordings
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
#'
#' @references
#'
#' Kosmidis, I., and Passfield, L. (2015). Linking the Performance of
#' Endurance Runners to Training and Physiological Effects via
#' Multi-Resolution Elastic Net. \emph{ArXiv e-print}
#' arXiv:1506.01388.
#'
#' Frick, H., Kosmidis, I. (2017). trackeR: Infrastructure for Running
#' and Cycling Data from GPS-Enabled Tracking Devices in
#' R. \emph{Journal of Statistical Software}, \bold{82}(7),
#' 1--29. doi:10.18637/jss.v082.i07
impute_speeds <- function(session_data, from_distances = TRUE,
                          lgap = 30, lskip = 5, m = 11,
                         cycling = FALSE, units = NULL) {

    if (length(session_data) < 2) {
        return(session_data)
    }

    if (is.null(units)) {
        units <- generate_base_units()
    }
    distUnit <- units$unit[units$variable == "distance"]
    speedUnits <- strsplit(units$unit[units$variable == "speed"], "_per_")[[1]]
    distUnitSpeed <- speedUnits[1]
    timeUnitSpeed <- switch(speedUnits[2], "s" = "secs", "min" = "mins", "h" = "hours", "d" = "days") ## README: can be avoided if we use the same names...

    ## Calculate speeds
    if (from_distances) {
        if (all(is.na(session_data$distance))) {
            warning("No distances are available to calculate the speeds. If available, measurements of speed are used instead.")
            ## check if speed data is available as an alternative, otherwise return session_data
            if (all(is.na(session_data$speed))) {
                return(session_data)
            }
        }
        else {
            session_data <- session_data[!is.na(session_data$distance)]
            if (distUnit != distUnitSpeed){
                conversion <- match.fun(paste(distUnit, distUnitSpeed, sep = "2"))

                dist <- conversion(coredata(session_data$distance))
            }
            else {
                dist <- coredata(session_data$distance)
            }
            session_data$speed <- distance2speed(dist, index(session_data), timeunit = timeUnitSpeed)
        }
    }
    else {
        if (all(is.na(session_data$speed))) {
            warning("No speeds are available. If available, distances are used to calculate speed.")
            if (!all(is.na(session_data$distance))) {
                session_data <- session_data[!is.na(session_data$distance)]
                if (distUnit != distUnitSpeed){
                    conversion <- match.fun(paste(distUnit, distUnitSpeed, sep = "2"))
                    dist <- conversion(coredata(session_data$distance))
                }
                else {
                    dist <- coredata(session_data$distance)
                }
                session_data$speed <- distance2speed(dist, index(session_data), timeunit = timeUnitSpeed)
            }
            else {
                return(session_data)
            }
        }
    }

    ## order variables for imputation:
    ## variables with 'content' imputation and variables with NA imputation
    originalOrder <- names(session_data)
    if (cycling){
        impC <- match(c("latitude", "longitude", "altitude", "distance", "speed", "power"), names(session_data))
        impN <- which(is.na(match(names(session_data), c("latitude", "longitude", "altitude", "distance", "speed", "power"))))
        impPower <- 0
        nN <- length(impN)
    }
    else {
        impC <- match(c("latitude", "longitude", "altitude", "distance", "speed"), names(session_data))
        impN <- which(is.na(match(names(session_data), c("latitude", "longitude", "altitude", "distance", "speed"))))
        impPower <- NA
        nN <- length(impN) - 1
    }
    session_data <- session_data[, c(impC, impN)]

    ## Remove observations with negative or missing speeds
    session_data <- session_data[session_data$speed >= 0 & !is.na(session_data$speed)]

    ## get session parts (which are separated by short breaks lasting more than lgap seconds)
    shortBreaks <- resting_periods(index(session_data), lgap/3600)

    ## Put some zeros within the short breaks
    #nObs <- nrow(session_data)
    nOther <- ncol(session_data) - 1
    nLaps <- nrow(shortBreaks$sessions)
    ## if there are more than 1 laps then impute zero speeds
    imputedData <- zoo(x = matrix(NA, nrow = 0, ncol = ncol(session_data),
                           dimnames = list(NULL, names(session_data))), order.by = as.POSIXct("1970-01-01")[c()])
    if (nLaps > 1) {
        for (j in seq.int(nLaps)[-nLaps]) {
            newtimes <- with(shortBreaks$sessions,
                             seq(sessionEnd[j] + lskip,
                                 sessionStart[j + 1] - lskip,
                                 length.out = m))
            newdata <- matrix(c(
                ## last know position
                as.vector(session_data[shortBreaks$sessions$sessionEnd[j], c("latitude", "longitude", "altitude")]),
                ## distance (will be updated based on imputed speeds)
                0,
                ## speed
                0,
                ## power
                impPower,
                ## anything else
                rep(NA, nN)), ncol = ncol(session_data),
                              dimnames = list(NULL, names(session_data)))
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
        as.vector(session_data[shortBreaks$sessions$sessionStart[1], c("latitude", "longitude", "altitude")]),
        ## distance (will be updated based on imputed speeds)
        0,
        ## speed
        0,
        ## power
        impPower,
        ## anything else
        rep(NA, nN)), ncol = ncol(session_data),
                           dimnames = list(NULL, names(session_data)))
    newtimesEnd <- seq(shortBreaks$sessions$sessionEnd[nLaps] + 1,
                       shortBreaks$sessions$sessionEnd[nLaps] + 5,
                       length = m)
    newdataEnd <- matrix(c(
        ## last know position
        as.vector(session_data[shortBreaks$sessions$sessionEnd[nLaps], c("latitude", "longitude", "altitude")]),
        ## distance (will be updated based on imputed speeds)
        0,
        ## speed
        0,
        ## power
        impPower,
        ## anything else
        rep(NA, nN)), ncol = ncol(session_data),
                         dimnames = list(NULL, names(session_data)))
    imputedData <- c(imputedData,
                     zoo(x = newdataStart, order.by = newtimesStart),
                     zoo(x = newdataEnd, order.by = newtimesEnd))
    session_data <- c(session_data, imputedData)


    ## update distances
    updatedDistance <- speed2distance(session_data$speed, index(session_data), timeunit = timeUnitSpeed)
    if (distUnit != distUnitSpeed) {
        conversion <- match.fun(paste(distUnitSpeed, distUnit, sep = "2"))
        updatedDistance <- conversion(updatedDistance)
    }
    session_data$distance <- zoo(updatedDistance, order.by = index(session_data)) ## cumsum doesn't return a zoo object


    ## clean up and return
    session_data <- session_data[, originalOrder]
    rownames(session_data) <- NULL
    return(session_data)
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



