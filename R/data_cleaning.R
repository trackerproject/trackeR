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
