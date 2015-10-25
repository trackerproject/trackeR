getAltitude <- function(object, country = NULL, mask = TRUE, ...){

    ## get ISO country code
    if (is.null(country)){
        firstLoc <- min(which(apply(object[,c("longitude", "latitude")], 1, function(x) !any(is.na(x)))))
        country <- ll2iso(lon = as.numeric(object$longitude[firstLoc]), lat = as.numeric(object$latitude[firstLoc]))
    }
    
    ## try to download altitude data
    rast <- try(raster::getData("alt", country = country, download = TRUE, mask = mask))
    ## In the case of alt you can set ’mask’ to FALSE. If it is TRUE values for
    ## neighbouring countries are set to NA.

    if (!inherits(rast, "try-error")){
        positionData <- data.frame(lng = object$longitude, lat = object$latitude)
        altitude <- raster::extract(rast, positionData, method = "bilinear")
    } else {
        ## otherwise use altitude from data set
        if ("altitude" %in% names(object)) {
            altitude <- object$altitude
        } else {
            stop("Altitude data could not be downloaded and no data is availble in the object itself.")
        }
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
    object$altitude <- getAltitude(object, country = country, mask = mask)
    
    ## correct GPS distances
    object$distance <- sqrt(object$distance^2 + object$altitude^2)

    return(object)
}


