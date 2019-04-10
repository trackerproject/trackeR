#' Thresholding for variables in \code{trackeRdata} objects
#'
#' @param object An object of class \code{\link{trackeRdata}}.
#' @param variable A vector containing the names of the variables to
#'     which thresholding is applied. See Details.
#' @param lower A vector containing the corresponding lower
#'     thresholds. See Details.
#' @param upper A vector containing the corresponding upper
#'     thresholds. See Details.
#' @param sport A vector of sports (amongst \code{'cycling'},
#'     \code{'running'}, \code{'swimming'}) with each element
#'     corresponding to \code{variable}, \code{lower} and \code{upper}
#' @param trace Should a progress report be printed? Default is \code{FALSE}
#' @param ... Currently not used.
#' @details
#'
#' \code{lower} and \code{upper} are always understood as referring to
#' the units of the \code{object}.
#'
#' If the arguments \code{variable}, \code{lower}, and \code{upper}
#' are all unspecified, the following default thresholds are employed
#' \itemize{
#' \item latitude [-90, 90] degrees
#' \item longitude [-180, 180] degrees
#' \item altitude [-500, 9000] m
#' \item distance [0, Inf] meters
#' \item cadence_running [0, Inf] steps per min
#' \item cadence_cycling [0, Inf] revolutions per min
#' \item speed [0, Inf] meters
#' \item heart rate [0, 250] bpm
#' \item power [0, Inf] W
#' \item pace [0, Inf] min per km
#' \item duration [0, Inf] seconds
#' \item temperature [-20, 60] C
#' }
#' after they have been tranformed to the units of the \code{object}
#'
#' The thresholds for speed differ across sports: for running they are
#' [0, 12.5] meters per second, for cycling [0, 100] meters per second
#' and for swimming [0, 5] meters per second.
#'
#'
#' @examples
#' \dontrun{
#' data('runs', package = 'trackeR')
#' plot(runs, session = 4, what = 'speed', threshold = FALSE)
#' runsT <- threshold(runs, variable = 'speed', lower = 0, upper = 12.5, sport = "running")
#' plot(runsT, session = 4, what = 'speed', threshold = FALSE)
#' }
#' @export
threshold.trackeRdata <- function(object,
                                  variable,
                                  lower,
                                  upper,
                                  sport,
                                  trace = FALSE,
                                  ...) {

    sports <- get_sport(object)
    units <- get_units(object)
    operations <- get_operations(object)

    ## if variable is NULL, just update attribute, leave data unchanged
    if (!missing(variable) && is.null(variable)) {
        operations <- get_operations(object)
        operations$threshold <- NULL
        attr(object, "operations") <- operations
        return(object)
    }

    no_variable <- missing(variable)
    no_unit <- missing(variable)
    no_sport <- missing(sport)

    ## Generate default thresholds
    thresholds <- generate_thresholds()
     ## Change default threshold units to the units of object
    thresholds <- change_units(thresholds, variable = units$variable, unit = units$unit, sport = units$sport)
    thresholds$changed <- FALSE

    if (!(no_sport & no_unit & no_variable)) {
        ## Assuming that lower and upper are supplied in the units of object
        ## This will also check if variable, lower, upper and sport have the right lengths
        thresholds_new <- generate_thresholds(variable, lower, upper, sport)
        thresholds_new$changed <- FALSE
        p <- length(variable)
        for (j in seq.int(p)) {
            ind <- thresholds_new$variable == variable[j] & thresholds_new$sport == sport[j]
            ## Set changed limits to their values
            thresholds[ind, ] <- thresholds_new[ind, ]
            thresholds[ind, "changed"] <- TRUE
        }
    }
    else {
        ## Assume that all are changed
        thresholds$changed <- TRUE
    }

    ## Change thresholds
    for (sp in unique(sports)) {
        th <- subset(thresholds, sport == sp)
        for (k in which(th$changed)) {
            va <- th$variable[k]
            ## trackeRdata objects do not carry duration so skip
            if (va == "duration") {
                next
            }
            if (isTRUE(trace)) {
                cat("Thresholding", va, "for", paste0(sp, "..."))
            }
            for (sess in which(sports == sp)) {
                inds_lower <- object[[sess]][, va] < th$lower[k]
                inds_upper <- object[[sess]][, va] > th$upper[k]
                object[[sess]][inds_lower, va] <- NA
                object[[sess]][inds_upper, va] <- NA
            }
            if (isTRUE(trace)) {
                cat(" Done!\n")
            }
        }
    }


    thresholds$changed <- NULL

    ## update attribute
    operations$threshold <- thresholds
    attr(object, "operations") <- operations

    return(object)
}

## not to be exported
get_units.trackeRthresholds <- function(object, ...) {
    object[, c("variable", "unit")]
}


#' Change the units of the variables in an \code{trackeRthresholds} object
#'
#' @param object An object of class \code{trackeRthresholds}.
#' @inheritParams change_units
#' @export
change_units.trackeRthresholds <- function(object,
                                           variable,
                                           unit,
                                           sport,
                                           ...) {
    no_variable <- missing(variable)
    no_unit <- missing(unit)
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
