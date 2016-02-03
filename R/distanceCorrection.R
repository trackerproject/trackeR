getAltitude <- function(object, country = NULL, mask = TRUE, ...){

    ## are any locations available?
    firstLoc <- min(which(apply(object[,c("longitude", "latitude")], 1, function(x) !any(is.na(x)))))
    if (!is.finite(firstLoc)) {
        stop("No location data available.")
    }
        
    ## get ISO country code
    if (is.null(country)){
        country <- ll2iso(lon = as.numeric(object$longitude[firstLoc]),
                          lat = as.numeric(object$latitude[firstLoc]))
    }
    
    ## try to download altitude data
    rast <- try(raster::getData("alt", country = country, download = TRUE, mask = mask))
    ## From documentation: "In the case of alt you can set 'mask' to FALSE. If it is TRUE values for
    ## neighbouring countries are set to NA."

    if (!inherits(rast, "try-error")){
        positionData <- data.frame(lng = object$longitude, lat = object$latitude)
        altitude <- raster::extract(rast, positionData, method = "bilinear")
    } else {
        stop("Altitude data could not be downloaded.")
    }
        
    return(as.numeric(altitude))
}

ll2iso <- function(lon, lat){    
    country <- as.character(ggmap::revgeocode(c(lon, lat), output = 'more')$country)
    ref <- data.frame(raster::getData('ISO3'), stringsAsFactors = FALSE)
    isocode <- ref$ISO3[ref$NAME == country]
    return(isocode)
}

distanceCorrection <- function(object, country = NULL, mask = TRUE, ...){
    ## get altitude data
    altitudeDwl <- try(getAltitude(object, country = country, mask = mask))
    if (!inherits(altitudeDwl, "try-error")) {
        object$altitude <- altitudeDwl
    }
    
    ## correct GPS distances if altitude information available
    if (!all(is.na(object$altitude))){ 
        object$distance <- cumsum(c(sqrt(diff(object$distance)^2 + diff(object$altitude)^2),0))
    } else {
        warning("No altitude information is available. Distances are not corrected for elevation.")
    }

    return(object)
}


