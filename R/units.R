#' Get the units of the variables in an \code{trackeRdata} object.
#'
#' @param object An object of class \code{\link{trackeRdata}}.
#' @param ... Currently not used.
#' @export
getUnits.trackeRdata <- function(object,...){
    attr(object, "units")
}

#' Get the units of the variables in an \code{trackeRdataSummary} object.
#'
#' @param object An object of class \code{trackeRdataSummary}.
#' @param ... Currently not used.
#' @export
getUnits.trackeRdataSummary <- function(object,...){
    attr(object, "units")
}

#' Get the units of the variables in an \code{distrProfile} object.
#'
#' @param object An object of class \code{distrProfile}.
#' @param ... Currently not used.
#' @export
getUnits.distrProfile <- function(object,...){
    attr(object, "units")
}

#' Get the units of the variables in an \code{conProfile} object.
#'
#' @param object An object of class \code{conProfile}.
#' @param ... Currently not used.
#' @export
getUnits.conProfile <- function(object,...){
    attr(object, "units")
}

#' Get the units of the variables in an \code{trackeRWprime} object.
#'
#' @param object An object of class \code{trackeRWprime}.
#' @param ... Currently not used.
#' @export
getUnits.trackeRWprime <- function(object,...){
    attr(object, "unit")
}

## not to be exported
getUnits.trackeRthresholds <- function(object, ...){
    object[, c("variable", "unit")]
}

getUnits.trackeRdataZones <- function(object, ...){
    attr(object, "units")
}

#' Change the units of the variables in an \code{trackeRdata} object.
#'
#' @param object An object of class \code{\link{trackeRdata}}.
#' @param variable A vector of variables to be changed.
#' @param unit A vector with the units, corresponding to variable.
#' @param ... Currently not used.
#' @export
changeUnits.trackeRdata <- function(object, variable, unit, ...){
    ## get current units and thresholds
    current <- getUnits(object)
    operations <- getOperations(object)
    th <- operations$threshold

    ## change units
    for (i in variable){
        currentUnit <- current$unit[current$variable == i]
        newUnit <- unit[which(variable == i)]
        if (currentUnit != newUnit) {
            conversion <- match.fun(paste(currentUnit, newUnit, sep = "2"))
            ## change data
            for (session in seq_along(object)){
                object[[session]][,i] <- conversion(object[[session]][,i])
            }

            ## change units attribute
            current$unit[current$variable == i] <- newUnit

            ## change units of thresholds
            th$lower[th$variable == i] <- conversion(th$lower[th$variable == i])
            th$upper[th$variable == i] <- conversion(th$upper[th$variable == i])
        }
    }

    ## update attributes and return
    attr(object, "units") <- current
    operations$threshold <- th
    attr(object, "operations") <- operations
    return(object)
}

#' Change the units of the variables in an \code{trackeRdataSummary} object.
#'
#' @param object An object of class \code{trackeRdataSummary}.
#' @param variable A vector of variables to be changed. Note, these are expected to be
#'     concepts like "speed" rather than variable names like "avgSpeed" or "avgSpeedMoving".
#' @param unit A vector with the units, corresponding to variable.
#' @param ... Currently not used.
#' @export
changeUnits.trackeRdataSummary <- function(object, variable, unit, ...){
    ## NOTE: variable is expected to contain concepts like "speed" rather than variable names like "avgSpeed" or "avgSpeedMoving".
    concept <- variable
    current <- getUnits(object)
    mvt <- attr(object, "movingThreshold")
    object <- as.data.frame(object)
    for (i in concept){
        variables <- names(object)[grep(pattern = i, names(object), ignore.case = TRUE)]
        currentUnit <- current$unit[current$variable == i] ## $concept
        newUnit <- unit[which(concept == i)]
        if (currentUnit != newUnit) {
            conversion <- match.fun(paste(currentUnit, newUnit, sep = "2"))
            ## convert summary statistics
            for (v in variables){
                object[,v] <- conversion(object[,v])
            }
            ## convert moving threshold
            if (i == "speed") mvt <- conversion(mvt)
            ## update units 
            current$unit[current$variable == i] <- newUnit
        }
        
    }

    ## update units attribute and return
    attr(object, "units") <- current
    attr(object, "movingThreshold") <- mvt
    class(object) <- c("trackeRdataSummary", class(object))
    return(object)
}    

#' Change the units of the variables in an \code{distrProfile} object.
#'
#' @param object An object of class \code{distrProfile} as returned by \code{\link{distributionProfile}}.
#' @param variable A vector of variables to be changed. 
#' @param unit A vector with the units, corresponding to variable.
#' @param ... Currently not used.
#' @export
changeUnits.distrProfile <- function(object, variable, unit, ...){
    current <- getUnits(object)
    
    ## change units
    for (i in variable){
        currentUnit <- current$unit[current$variable == i]
        newUnit <- unit[which(variable == i)]
        if (currentUnit != newUnit) {
            conversion <- match.fun(paste(currentUnit, newUnit, sep = "2"))
            ## change grid
            newIndex <- conversion(index(object[[i]]))
            object[[i]] <- zoo(coredata(object[[i]]), order.by = newIndex)
            ## change units attribute
            current$unit[current$variable == i] <- newUnit
        }
    }

    ## update attributes and return
    attr(object, "units") <- current
    return(object)
}

#' Change the units of the variables in an \code{conProfile} object.
#'
#' @param object An object of class \code{conProfile} as returned by \code{\link{concentrationProfile}}.
#' @param variable A vector of variables to be changed. 
#' @param unit A vector with the units, corresponding to variable.
#' @param ... Currently not used.
#' @export
changeUnits.conProfile <- function(object, variable, unit, ...){
    current <- getUnits(object)
    
    ## change units
    for (i in variable){
        currentUnit <- current$unit[current$variable == i]
        newUnit <- unit[which(variable == i)]
        if (currentUnit != newUnit) {
            conversion <- match.fun(paste(currentUnit, newUnit, sep = "2"))
            ## change grid
            newIndex <- conversion(index(object[[i]]))
            object[[i]] <- zoo(coredata(object[[i]]), order.by = newIndex)
            ## change units attribute
            current$unit[current$variable == i] <- newUnit
        }
    }

    ## update attributes and return
    attr(object, "units") <- current
    return(object)
}

#' Change the units of the variables in an \code{trackeRdata} object.
#'
#' @param object An object of class \code{\link{trackeRdata}}.
#' @param variable A vector of variables to be changed.
#' @param unit A vector with the units, corresponding to variable.
#' @param ... Currently not used.
#' @export
changeUnits.trackeRWprime <- function(object, variable, unit, ...){
    ## get current unit
    current <- getUnits(object)

    if (missing(variable)) variable <- ifelse(attr(object, "cycling"), "power", "speed")
    if (missing(unit) & !missing(variable)) {
        unit <- variable
        variable <- ifelse(attr(object, "cycling"), "power", "speed")
    }
    if (attr(object, "cycling")){
        if(variable != "power") stop("Can only change measurement units for power.")
    } else {
        if(variable != "speed") stop("Can only change measurement units for speed.")
    }
    
    ## change units
    for (i in variable){
        currentUnit <- current$unit[current$variable == i]
        newUnit <- unit[which(variable == i)]
        if (currentUnit != newUnit) {
            conversion <- match.fun(paste(currentUnit, newUnit, sep = "2"))
            ## change data
            for (session in seq_along(object)){
                object[[session]][,"movement"] <- conversion(object[[session]][,"movement"])
            }
            ## change units attribute
            current$unit[current$variable == i] <- newUnit
        }
    }

    ## update attributes and return
    attr(object, "units") <- current
    return(object)
}

## not to be exported
changeUnits.trackeRthresholds <- function(object, variable, unit, ...){

    for (v in variable){
        i <- which(object$variable == v)
        currentUnit <- object$unit[i]
        newUnit <- unit[which(variable == v)]
        if (currentUnit != newUnit) {
            conversion <- match.fun(paste(currentUnit, newUnit, sep = "2"))
            object$lower[i] <- conversion(object$lower[i])
            object$upper[i] <- conversion(object$upper[i])
            object$unit[i] <- newUnit            
        }
    }
    return(object)
}


changeUnits.trackeRdataZones <- function(object, variable, unit, ...){

    current <- getUnits(object)
    
    ## change units
    for (i in variable){
        currentUnit <- current$unit[current$variable == i]
        newUnit <- unit[which(variable == i)]
        if (currentUnit != newUnit) {
            conversion <- match.fun(paste(currentUnit, newUnit, sep = "2"))
            ## change zone limits
            object[[i]]$lower <- conversion(object[[i]]$lower)
            object[[i]]$upper <- conversion(object[[i]]$upper)
            ## change units attribute
            current$unit[current$variable == i] <- newUnit
        }
    }

    ## update attributes and return
    attr(object, "units") <- current
    return(object)    
}






#' Get the operation settings of an \code{trackeRdata} object.
#'
#' @param object An object of class \code{\link{trackeRdata}}.
#' @param ... Currently not used.
#' @export
getOperations.trackeRdata <- function(object,...){
    attr(object, "operations")
}

#' Get the operation settings of an \code{distrProfile} object.
#'
#' @param object An object of class \code{distrProfile} as returned by \code{\link{distributionProfile}}.
#' @param ... Currently not used.
#' @export
getOperations.distrProfile <- function(object,...){
    attr(object, "operations")
}

#' Get the operation settings of an \code{conProfile} object.
#'
#' @param object An object of class \code{conProfile} as returned by \code{\link{concentrationProfile}}.
#' @param ... Currently not used.
#' @export
getOperations.conProfile <- function(object,...){
    attr(object, "operations")
}





## README: export?
#' Generate base units.
#'
#' @param cycling Logical. Is the data from a cycling session rather than a running session?
#' @param ... Currently not used.
generateBaseUnits <- function(cycling = FALSE, ...){
    ## Get the variable names
    varnames <- generateVariableNames()$humanNames
    ## Remove time and add duration
    varnames <- varnames[-match("time", varnames)]
    varnames <- c(varnames, c("pace","duration"))

    if (cycling) {
        units <- c("degree", "degree", "m", "m", "bpm", "m_per_s", "rev_per_min", "W", "min_per_km", "s")
    } else {
        units <- c("degree", "degree", "m", "m", "bpm", "m_per_s", "steps_per_min", "W", "min_per_km", "s")
    }

    return(data.frame(variable = varnames, unit = units, stringsAsFactors = FALSE))
}


## conversion functions: distance
#' Auxiliary conversion functions.
#'
#' Conversion functions for distance, duration, speed, pace, power, and cadence.
#'
#' @param variable Variable to be converted.
#' @name conversions
#' @export
m2km <- function(variable){
    variable / 1000
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
km2m <- function(variable){
    variable * 1000
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
m2ft <- function(variable){
    variable * 3937/1200
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
ft2m <- function(variable){
    variable * 1200/3937
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
m2mi <- function(variable){
    variable / 1609.344 
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
mi2m <- function(variable){
    variable * 1.609344 * 1000
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
km2ft <- function(variable){
    variable / 1.609344 * 5280
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
ft2km <- function(variable){
    variable * 1.609344 / 5280
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
km2mi <- function(variable){
    variable / 1.609344
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
mi2km <- function(variable){
    variable * 1.609344
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
ft2mi <- function(variable){
    variable / 5280
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
mi2ft <- function(variable){
    variable * 5280
}

#' @inheritParams conversions
#' @rdname conversions
#' @export
m2m <- function(variable){
    variable
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
km2km <- function(variable){
    variable
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
ft2ft <- function(variable){
    variable
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
mi2mi <- function(variable){
    variable
}


## conversion functions: duration
#' @inheritParams conversions
#' @rdname conversions
#' @export
s2min <- function(variable){
    if (class(variable) == "difftime") {
        units(variable) <- "mins"
    } else {
        variable <- variable / 60
    }
    return(variable)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
min2s <- function(variable){
    if (class(variable) == "difftime") {
        units(variable) <- "secs"
    } else {
        variable <- variable * 60
    }
    return(variable)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
s2h <- function(variable){
    if (class(variable) == "difftime") {
        units(variable) <- "hours"
    } else {
        variable <- variable / 60 / 60
    }
    return(variable)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
h2s <- function(variable){
    if (class(variable) == "difftime") {
        units(variable) <- "secs"
    } else {
        variable <- variable * 60 * 60
    }
    return(variable)    
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
min2h <- function(variable){
    if (class(variable) == "difftime") {
        units(variable) <- "hours"
    } else {
        variable <- variable / 60
    }
    return(variable)    
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
h2min <- function(variable){
    if (class(variable) == "difftime") {
        units(variable) <- "mins"
    } else {
        variable <- variable * 60
    }
    return(variable)    
}

#' @inheritParams conversions
#' @rdname conversions
#' @export
s2s <- min2min <- h2h <- function(variable){
    variable
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
min2min <- function(variable){
    variable
}

#' @inheritParams conversions
#' @rdname conversions
#' @export
h2h <- function(variable){
    variable
}





## conversion functions: speed
#' @inheritParams conversions
#' @rdname conversions
#' @export
m_per_s2km_per_h <- function(variable){
    variable / 1000 * 60 * 60
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
km_per_h2m_per_s <- function(variable){
    variable * 1000 / 60 / 60
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
m_per_s2ft_per_min <- function(variable){
    variable  * 3937/1200 * 60
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
ft_per_min2m_per_s <- function(variable){
    variable  / (3937/1200) / 60
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
m_per_s2ft_per_s <- function(variable){
    variable * 3937/1200
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
ft_per_s2m_per_s <- function(variable){
    variable / (3937/1200)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
m_per_s2mi_per_h <- function(variable){
    variable  / 1609.344 * 60 * 60
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
mi_per_h2m_per_s <- function(variable){
    variable * 1609.344 / 60 / 60
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
m_per_s2km_per_min <- function(variable){
    m_per_s2km_per_h(variable) / 60
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
km_per_min2m_per_s <- function(variable){
    km_per_h2m_per_s(variable * 60)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
m_per_s2mi_per_min <- function(variable){
    m_per_s2mi_per_h(variable) / 60
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
mi_per_min2m_per_s <- function(variable){
    mi_per_h2m_per_s(variable * 60)
}


#' @inheritParams conversions
#' @rdname conversions
#' @export
km_per_h2ft_per_min <- function(variable){
    km2ft(variable / 60)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
ft_per_min2km_per_h <- function(variable){
    ft2km(variable * 60)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
km_per_h2ft_per_s <- function(variable){
    km2ft(variable / 60 / 60)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
ft_per_s2km_per_h <- function(variable){
    ft2km(variable * 60 * 60)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
km_per_h2mi_per_h <- function(variable){
    km2mi(variable)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
mi_per_h2km_per_h <- function(variable){
    mi2km(variable)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
km_per_h2km_per_min <- function(variable){
    variable / 60
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
km_per_min2km_per_h <- function(variable){
    variable * 60
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
km_per_h2mi_per_min <- function(variable){
    km_per_h2mi_per_h(variable) / 60
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
mi_per_min2km_per_h <- function(variable){
    mi_per_h2km_per_h(variable * 60)
}



#' @inheritParams conversions
#' @rdname conversions
#' @export
ft_per_min2ft_per_s <- function(variable){
    variable / 60
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
ft_per_s2ft_per_min <- function(variable){
    variable * 60
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
ft_per_min2mi_per_h <- function(variable){
    ft2mi(variable * 60)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
mi_per_h2ft_per_min <- function(variable){
    mi2ft(variable / 60)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
ft_per_min2km_per_min <- function(variable){
   ft2km(variable)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
km_per_min2ft_per_min <- function(variable){
   km2ft(variable)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
ft_per_min2mi_per_min <- function(variable){
    ft2mi(variable)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
mi_per_min2ft_per_min <- function(variable){
    mi2ft(variable)
}


#' @inheritParams conversions
#' @rdname conversions
#' @export
ft_per_s2mi_per_h <- function(variable){
    ft2mi(variable * 60 * 60)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
mi_per_h2ft_per_s <- function(variable){
    mi2ft(variable / 60 / 60)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
ft_per_s2km_per_min <- function(variable){
   ft2km(variable * 60)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
km_per_min2ft_per_s <- function(variable){
   km2ft(variable / 60)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
ft_per_s2mi_per_min <- function(variable){
    ft2mi(variable * 60)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
mi_per_min2ft_per_s <- function(variable){
    mi2ft(variable / 60)
}


#' @inheritParams conversions
#' @rdname conversions
#' @export
mi_per_h2km_per_min <- function(variable){
   mi2km(variable / 60)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
km_per_min2mi_per_h <- function(variable){
   km2mi(variable * 60)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
mi_per_h2mi_per_min <- function(variable){
   variable / 60
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
mi_per_min2mi_per_h <- function(variable){
    variable * 60
}


#' @inheritParams conversions
#' @rdname conversions
#' @export
km_per_min2mi_per_min <- function(variable){
    km2mi(variable)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
mi_per_min2km_per_min <- function(variable){
    mi2km(variable)
}




#' @inheritParams conversions
#' @rdname conversions
#' @export
m_per_s2m_per_s <- function(variable){
    variable
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
km_per_h2km_per_h <- function(variable){
    variable
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
ft_per_min2ft_per_min <- function(variable){
    variable
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
ft_per_s2ft_per_s <- function(variable){
    variable
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
mi_per_h2mi_per_h <- function(variable){
    variable
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
km_per_min2km_per_min <- function(variable){
    variable
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
mi_per_min2mi_per_min <- function(variable){
    variable
}





## conversion functions: pace
#' @inheritParams conversions
#' @rdname conversions
#' @export
s_per_m2min_per_km <- function(variable){
    variable * 1000 / 60
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
min_per_km2s_per_m <- function(variable){
    variable / 1000 * 60
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
s_per_m2min_per_mi <- function(variable){
    variable / 60 * 1609.344 
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
min_per_mi2s_per_m <- function(variable){
    variable * 60 / 1609.344 
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
min_per_km2min_per_mi <- function(variable){
    variable * 1.609344
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
min_per_mi2min_per_km <- function(variable){
    variable / 1.609344
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
s_per_m2s_per_m <- function(variable){
    variable
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
min_per_km2min_per_km <- function(variable){
    variable
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
min_per_mi2min_per_mi <- function(variable){
    variable
}


## conversion functions: power
#' @inheritParams conversions
#' @rdname conversions
#' @export
W2kW <- function(variable){
    variable / 1000
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
kW2W <- function(variable){
    variable * 1000
}

#' @inheritParams conversions
#' @rdname conversions
#' @export
W2W <- function(variable){
    variable
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
kW2kW <- function(variable){
    variable
}


## conversion functions: cadence
#' @inheritParams conversions
#' @rdname conversions
#' @export
steps_per_min2steps_per_min <- function(variable){
    variable 
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
rev_per_min2rev_per_min <- function(variable){
    variable 
}
## steps_per_min2rev_per_min <- rev_per_min2steps_per_min <- function(variable){
##     variable 
## }
