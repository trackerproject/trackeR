#' Get the units of the variables in an \code{trackeRdata} object
#'
#' @param object An object of class \code{\link{trackeRdata}}.
#' @param ... Currently not used.
#' @export
get_units.trackeRdata <- function(object, ...) {
    attr(object, "units")
}

#' Get the units of the variables in an \code{trackeRdataSummary} object
#'
#' @param object An object of class \code{trackeRdataSummary}.
#' @param ... Currently not used.
#' @export
get_units.trackeRdataSummary <- function(object, ...) {
    attr(object, "units")
}

#' Get the units of the variables in an \code{distrProfile} object
#'
#' @param object An object of class \code{distrProfile}.
#' @param ... Currently not used.
#' @export
get_units.distrProfile <- function(object, ...) {
    attr(object, "units")
}

#' Get the units of the variables in an \code{conProfile} object
#'
#' @param object An object of class \code{conProfile}.
#' @param ... Currently not used.
#' @export
get_units.conProfile <- function(object, ...) {
    attr(object, "units")
}

#' Get the units of the variables in an \code{trackeRWprime} object
#'
#' @param object An object of class \code{trackeRWprime}.
#' @param ... Currently not used.
#' @export
get_units.trackeRWprime <- function(object, ...) {
    attr(object, "unit")
}

## not to be exported
get_units.trackeRthresholds <- function(object, ...) {
    object[, c("variable", "unit")]
}

get_units.trackeRdataZones <- function(object, ...) {
    attr(object, "units")
}

get_units.trackeRfpca <- function(object, ...) {
    attr(object, "units")
}


## not to be exported
change_units.trackeRthresholds <- function(object,
                                           variable,
                                           unit,
                                           sport,
                                           ...) {
    no_variable <- missing(variable)
    no_unit <- missing(variable)
    no_sport <- missing(sport)

    if (no_sport & no_unit & no_variable) {
        return(object)
    }
    else {
        p <- length(sport)
        if (length(unit) == p & length(variable) == p) {
            inputs <- data.frame(sport = sport, variable = variable, unit = unit, stringsAsFactors = FALSE)
            inds <- match(paste(inputs$sport, inputs$variable, sep = "-"),
                          paste(object$sport, object$variable, sep = "-"),
                          nomatch = 0)
            object$new_unit <- object$unit
            ## If variable/sport/units combinations do not exist then the object is returned
            if (all(inds == 0)) {
                stop("some of the supplied combinations of sport and variable do not exist.")
            }

            object$new_unit[inds] <- inputs$unit
            object$fun <- paste(object$unit, object$new_unit, sep = "2")

            ## Check for crappy units is inherent below
            for (i in seq.int(nrow(object))) {
                convert <- match.fun(object$fun[i])
                object[i, "lower"] <- convert(object[i, "lower"])
                object[i, "upper"] <- convert(object[i, "upper"])
            }
            object$unit <- object$new_unit
            object$fun <- object$new_unit <- NULL
            return(object)
        }
        else {
            stop("variable, unit and sport should have the same length.")
        }
    }
}


#' Change the units of the variables in an \code{trackeRdata} object
#'
#' @param object An object of class \code{\link{trackeRdata}}.
#' @inheritParams change_units
#' @export
change_units.trackeRdata <- function(object,
                                     variable,
                                     unit,
                                     sport,
                                     ...) {
    ## get current units and thresholds
    units <- get_units(object)
    operations <- get_operations(object)
    sports <- get_sport(object)

    is_na_sports <- is.na(sport)
    if (any(is.na(sports))) {
        stop("cannot change units. The sport for sessions", which(is_na_sports), "has not been identified. See ?set_sport on how to set a sport for those sessions.")
    }

    th <- operations$threshold

    no_variable <- missing(variable)
    no_unit <- missing(variable)
    no_sport <- missing(sport)

    if (no_sport & no_unit & no_variable) {
        return(object)
    }
    else {
        p <- length(sport)
        if (length(unit) == p & length(variable) == p) {
            inputs <- data.frame(sport = sport, variable = variable, unit = unit, stringsAsFactors = FALSE)
            inds <- match(paste(inputs$sport, inputs$variable, sep = "-"),
                          paste(units$sport, units$variable, sep = "-"),
                          nomatch = 0)
            units$new_unit <- units$unit
            ## If variable/sport/units combinations do not exist then the object is returned
            if (all(inds == 0)) {
                stop("some of the supplied combinations of sport and variable do not exist.")
            }

            units$new_unit[inds] <- inputs$unit
            ## Remove duration (only for trackeRdataSummary objects)
            ## units <- units[!(units$variable == "duration"), ]
            units$fun <- paste(units$unit, units$new_unit, sep = "2")
            units$changed <- units$unit != units$new_unit

            ## Check for crappy units
            ch <- sapply(units$fun, match.fun)

            for (sp in unique(sports)) {
                un <- subset(units, sport == sp)
                for (k in which(un$changed)) {
                    convert <- match.fun(un$fun[k])
                    va <- un$variable[k]
                    ## Do thresholds if they exist
                    if (!is.null(th)) {
                        th[th$sport == sp & th$variable == va, "lower"] <-
                            convert(th[th$sport == sp & th$variable == va, "lower"])
                        th[th$sport == sp & th$variable == va, "upper"] <-
                            convert(th[th$sport == sp & th$variable == va, "upper"])
                        th[th$sport == sp & th$variable == va, "unit"] <-
                            un$new_unit[k]
                    }
                    ## trackeRdata objects do not carry duration so skip
                    if (va == "duration") {
                        next
                    }
                    for (sess in which(sports == sp)) {
                        object[[sess]][, va] <- convert(object[[sess]][, va])
                    }
                }
            }

            ## Clean up units
            units$unit <- units$new_unit
            units$fun <- units$new_unit <- units$changed <- NULL

            ## update attributes and return
            attr(object, "units") <- units
            operations$threshold <- th
            attr(object, "operations") <- operations
            return(object)

        }
        else {
            stop("variable, unit and sport should have the same length.")
        }
    }
}


#' Change the units of the variables in an \code{trackeRdataSummary} object
#'
#' @param object An object of class \code{trackeRdataSummary}.
#' @param variable A vector of variables to be changed. Note, these are expected to be
#'     concepts like 'speed' rather than variable names like 'avgSpeed' or 'avgSpeedMoving'.
#' @param unit A vector with the units, corresponding to variable.
#' @param ... Currently not used.
#' @export
change_units.trackeRdataSummary <- function(object,
                                            variable,
                                            unit,
                                            ...) {
    ## NOTE: variable is expected to contain concepts like 'speed' rather than variable
    ## names like 'avgSpeed' or 'avgSpeedMoving'.
    concept <- variable
    units <- get_units(object)
    current <- collect_units(units, unit_reference_sport = attr(object, "unit_reference_sport"))

    mt <- attr(object, "moving_threshold")
    object <- as.data.frame(object)

    for (i in concept) {
        variables <- names(object)[grep(pattern = i, names(object), ignore.case = TRUE)]
        currentUnit <- current$unit[current$variable == i]  ## $concept
        newUnit <- unit[which(concept == i)]
        if (currentUnit != newUnit) {
            conversion <- match.fun(paste(currentUnit, newUnit, sep = "2"))
            ## convert summary statistics
            for (v in variables) {
                object[, v] <- conversion(object[, v])
            }
            ## convert moving threshold
            if (i == "speed")
                mt <- conversion(mt)
            ## update units
            current$unit[current$variable == i] <- newUnit
        }

    }

    for (va in current$variable) {
        units$unit[units$variable == va] <- current$unit[current$variable == va]
    }

    ## update units attribute and return
    attr(object, "units") <- units
    attr(object, "moving_threshold") <- mt
    class(object) <- c("trackeRdataSummary", class(object))
    return(object)
}

change_units.trackeRdataZones <- function(object,
                                          variable,
                                          unit,
                                          ...) {

    current <- getUnits(object)

    ## change units
    for (i in variable) {
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


#' Change the units of the variables in an \code{distrProfile} object
#'
#' @param object An object of class \code{distrProfile} as returned by \code{\link{distributionProfile}}.
#' @param variable A vector of variables to be changed.
#' @param unit A vector with the units, corresponding to variable.
#' @param ... Currently not used.
#' @export
change_units.distrProfile <- function(object,
                                      variable,
                                      unit,
                                      ...) {
    current <- getUnits(object)

    ## change units
    for (i in variable) {
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

#' Change the units of the variables in an \code{conProfile} object
#'
#' @param object An object of class \code{conProfile} as returned by \code{\link{concentrationProfile}}.
#' @param variable A vector of variables to be changed.
#' @param unit A vector with the units, corresponding to variable.
#' @param ... Currently not used.
#' @export
change_units.conProfile <- function(object,
                                    variable,
                                    unit,
                                    ...) {
    current <- getUnits(object)

    ## change units
    for (i in variable) {
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

#' Change the units of the variables in an \code{trackeRdata} object
#'
#' @param object An object of class \code{\link{trackeRdata}}.
#' @param variable A vector of variables to be changed.
#' @param unit A vector with the units, corresponding to variable.
#' @param ... Currently not used.
#' @export
change_units.trackeRWprime <- function(object,
                                       variable,
                                       unit,
                                       ...) {
    ## get current unit
    current <- getUnits(object)

    if (missing(variable))
        variable <- ifelse(attr(object, "cycling"), "power", "speed")
    if (missing(unit) & !missing(variable)) {
        unit <- variable
        variable <- ifelse(attr(object, "cycling"), "power", "speed")
    }
    if (attr(object, "cycling")) {
        if (variable != "power")
            stop("can only change measurement units for power.")
    } else {
        if (variable != "speed")
            stop("can only change measurement units for speed.")
    }

    ## change units
    for (i in variable) {
        currentUnit <- current$unit[current$variable == i]
        newUnit <- unit[which(variable == i)]
        if (currentUnit != newUnit) {
            conversion <- match.fun(paste(currentUnit, newUnit, sep = "2"))
            ## change data
            for (session in seq_along(object)) {
                object[[session]][, "movement"] <- conversion(object[[session]][, "movement"])
            }
            ## change units attribute
            current$unit[current$variable == i] <- newUnit
        }
    }

    ## update attributes and return
    attr(object, "units") <- current
    return(object)
}


#' Get the operation settings of an \code{trackeRdata} object
#'
#' @param object An object of class \code{\link{trackeRdata}}.
#' @param ... Currently not used.
#' @export
get_operations.trackeRdata <- function(object, ...) {
    attr(object, "operations")
}

#' Get the operation settings of an \code{distrProfile} object
#'
#' @param object An object of class \code{distrProfile} as returned by \code{\link{distributionProfile}}.
#' @param ... Currently not used.
#' @export
get_operations.distrProfile <- function(object, ...) {
    attr(object, "operations")
}

#' Get the operation settings of an \code{conProfile} object
#'
#' @param object An object of class \code{conProfile} as returned by \code{\link{concentrationProfile}}.
#' @param ... Currently not used.
#' @export
get_operations.conProfile <- function(object, ...) {
    attr(object, "operations")
}

## conversion functions: distance
#' Auxiliary conversion functions
#'
#' Conversion functions for distance, duration, speed, pace, power,
#' cadence and temperature.
#'
#' @param variable Variable to be converted.
#' @name conversions
#' @export
m2km <- function(variable) {
    variable/1000
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
km2m <- function(variable) {
    variable * 1000
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
m2ft <- function(variable) {
    variable * 3937/1200
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
ft2m <- function(variable) {
    variable * 1200/3937
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
m2mi <- function(variable) {
    variable/1609.344
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
mi2m <- function(variable) {
    variable * 1.609344 * 1000
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
km2ft <- function(variable) {
    variable/1.609344 * 5280
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
ft2km <- function(variable) {
    variable * 1.609344/5280
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
km2mi <- function(variable) {
    variable/1.609344
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
mi2km <- function(variable) {
    variable * 1.609344
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
ft2mi <- function(variable) {
    variable/5280
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
mi2ft <- function(variable) {
    variable * 5280
}

#' @inheritParams conversions
#' @rdname conversions
#' @export
m2m <- function(variable) {
    variable
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
km2km <- function(variable) {
    variable
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
ft2ft <- function(variable) {
    variable
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
mi2mi <- function(variable) {
    variable
}


## conversion functions: duration
#' @inheritParams conversions
#' @rdname conversions
#' @export
s2min <- function(variable) {
    if (class(variable) == "difftime") {
        units(variable) <- "mins"
    } else {
        variable <- variable/60
    }
    return(variable)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
min2s <- function(variable) {
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
s2h <- function(variable) {
    if (class(variable) == "difftime") {
        units(variable) <- "hours"
    } else {
        variable <- variable/60/60
    }
    return(variable)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
h2s <- function(variable) {
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
min2h <- function(variable) {
    if (class(variable) == "difftime") {
        units(variable) <- "hours"
    } else {
        variable <- variable/60
    }
    return(variable)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
h2min <- function(variable) {
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
s2s <- min2min <- h2h <- function(variable) {
    variable
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
min2min <- function(variable) {
    variable
}

#' @inheritParams conversions
#' @rdname conversions
#' @export
h2h <- function(variable) {
    variable
}

## conversion functions: degree
#' @inheritParams conversions
#' @rdname conversions
#' @export
degree2degree <- function(variable) {
    variable
}




## conversion functions: speed
#' @inheritParams conversions
#' @rdname conversions
#' @export
m_per_s2km_per_h <- function(variable) {
    variable/1000 * 60 * 60
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
km_per_h2m_per_s <- function(variable) {
    variable * 1000/60/60
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
m_per_s2ft_per_min <- function(variable) {
    variable * 3937/1200 * 60
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
ft_per_min2m_per_s <- function(variable) {
    variable/(3937/1200)/60
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
m_per_s2ft_per_s <- function(variable) {
    variable * 3937/1200
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
ft_per_s2m_per_s <- function(variable) {
    variable/(3937/1200)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
m_per_s2mi_per_h <- function(variable) {
    variable/1609.344 * 60 * 60
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
mi_per_h2m_per_s <- function(variable) {
    variable * 1609.344/60/60
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
m_per_s2km_per_min <- function(variable) {
    m_per_s2km_per_h(variable)/60
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
km_per_min2m_per_s <- function(variable) {
    km_per_h2m_per_s(variable * 60)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
m_per_s2mi_per_min <- function(variable) {
    m_per_s2mi_per_h(variable)/60
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
mi_per_min2m_per_s <- function(variable) {
    mi_per_h2m_per_s(variable * 60)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
km_per_h2ft_per_min <- function(variable) {
    km2ft(variable/60)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
ft_per_min2km_per_h <- function(variable) {
    ft2km(variable * 60)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
km_per_h2ft_per_s <- function(variable) {
    km2ft(variable/60/60)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
ft_per_s2km_per_h <- function(variable) {
    ft2km(variable * 60 * 60)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
km_per_h2mi_per_h <- function(variable) {
    km2mi(variable)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
mi_per_h2km_per_h <- function(variable) {
    mi2km(variable)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
km_per_h2km_per_min <- function(variable) {
    variable/60
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
km_per_min2km_per_h <- function(variable) {
    variable * 60
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
km_per_h2mi_per_min <- function(variable) {
    km_per_h2mi_per_h(variable)/60
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
mi_per_min2km_per_h <- function(variable) {
    mi_per_h2km_per_h(variable * 60)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
ft_per_min2ft_per_s <- function(variable) {
    variable/60
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
ft_per_s2ft_per_min <- function(variable) {
    variable * 60
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
ft_per_min2mi_per_h <- function(variable) {
    ft2mi(variable * 60)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
mi_per_h2ft_per_min <- function(variable) {
    mi2ft(variable/60)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
ft_per_min2km_per_min <- function(variable) {
    ft2km(variable)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
km_per_min2ft_per_min <- function(variable) {
    km2ft(variable)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
ft_per_min2mi_per_min <- function(variable) {
    ft2mi(variable)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
mi_per_min2ft_per_min <- function(variable) {
    mi2ft(variable)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
ft_per_s2mi_per_h <- function(variable) {
    ft2mi(variable * 60 * 60)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
mi_per_h2ft_per_s <- function(variable) {
    mi2ft(variable/60/60)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
ft_per_s2km_per_min <- function(variable) {
    ft2km(variable * 60)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
km_per_min2ft_per_s <- function(variable) {
    km2ft(variable/60)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
ft_per_s2mi_per_min <- function(variable) {
    ft2mi(variable * 60)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
mi_per_min2ft_per_s <- function(variable) {
    mi2ft(variable/60)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
mi_per_h2km_per_min <- function(variable) {
    mi2km(variable/60)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
km_per_min2mi_per_h <- function(variable) {
    km2mi(variable * 60)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
mi_per_h2mi_per_min <- function(variable) {
    variable/60
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
mi_per_min2mi_per_h <- function(variable) {
    variable * 60
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
km_per_min2mi_per_min <- function(variable) {
    km2mi(variable)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
mi_per_min2km_per_min <- function(variable) {
    mi2km(variable)
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
m_per_s2m_per_s <- function(variable) {
    variable
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
km_per_h2km_per_h <- function(variable) {
    variable
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
ft_per_min2ft_per_min <- function(variable) {
    variable
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
ft_per_s2ft_per_s <- function(variable) {
    variable
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
mi_per_h2mi_per_h <- function(variable) {
    variable
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
km_per_min2km_per_min <- function(variable) {
    variable
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
mi_per_min2mi_per_min <- function(variable) {
    variable
}


## conversion functions: heart_rate
#' @inheritParams conversions
#' @rdname conversions
#' @export
bpm2bpm <- function(variable) {
    variable
}


## conversion functions: pace
#' @inheritParams conversions
#' @rdname conversions
#' @export
s_per_m2min_per_km <- function(variable) {
    variable * 1000/60
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
min_per_km2s_per_m <- function(variable) {
    variable/1000 * 60
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
s_per_m2min_per_mi <- function(variable) {
    variable/60 * 1609.344
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
min_per_mi2s_per_m <- function(variable) {
    variable * 60/1609.344
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
min_per_km2min_per_mi <- function(variable) {
    variable * 1.609344
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
min_per_mi2min_per_km <- function(variable) {
    variable/1.609344
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
s_per_m2s_per_m <- function(variable) {
    variable
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
min_per_km2min_per_km <- function(variable) {
    variable
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
min_per_mi2min_per_mi <- function(variable) {
    variable
}
h_per_km2min_per_km <- function(variable) {
    variable * 60
}
h_per_mi2min_per_km <- function(variable) {
    variable * 60 / mi2_km(1)
}
h_per_mi2min_per_mi <- function(variable) {
    variable * 60
}


## conversion functions: power
#' @inheritParams conversions
#' @rdname conversions
#' @export
W2kW <- function(variable) {
    variable/1000
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
kW2W <- function(variable) {
    variable * 1000
}

#' @inheritParams conversions
#' @rdname conversions
#' @export
W2W <- function(variable) {
    variable
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
kW2kW <- function(variable) {
    variable
}


## conversion functions: cadence
#' @inheritParams conversions
#' @rdname conversions
#' @export
steps_per_min2steps_per_min <- function(variable) {
    variable
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
rev_per_min2rev_per_min <- function(variable) {
    variable
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
steps_per_min2rev_per_min <- function(variable) {
    ## step defined as half a revolution
    variable/2
}
#' @inheritParams conversions
#' @rdname conversions
#' @export
rev_per_min2steps_per_min <- function(variable) {
    ## step defined as half a revolution
    variable * 2
}

## conversion functions: temperature
#' @inheritParams conversions
#' @rdname conversions
#' @export
C2F <- function(variable) {
    variable * 9 / 5 + 32
}

#' @inheritParams conversions
#' @rdname conversions
#' @export
C2C <- function(variable) {
    variable
}

#' @inheritParams conversions
#' @rdname conversions
#' @export
F2F <- function(variable) {
    variable
}

#' @inheritParams conversions
#' @rdname conversions
#' @export
F2C <- function(variable) {
    (variable - 32) * 5 / 9
}

