#' Thresholding for variables in \code{trackeRdata} objects.
#'
#' @param object An object of class \code{\link{trackeRdata}}.
#' @param variable A vector containing the names of the variables to which thresholding is applied. See Details.
#' @param lower A vector containing the corresponding lower thresholds. See Details.
#' @param upper A vector containing the corresponding upper thresholds. See Details.
#' @param ... Currently not used.
#' @details Argument \code{variable} can also be a data frame containing the variable names, lower, and upper thresholds.
#' If arguments \code{variable}, \code{lower}, and \code{upper} are all unspecified, the following default thresholds are employed: latitude [-90, 90] degrees, longitude [-180, 180] degrees, altitude [-500, 9000] m, distance [0, Inf] meters, heart rate [0, 250] bpm, power [0, Inf] W, pace [0, Inf] min per km, duration [0, Inf] seconds. The thresholds for speed differ for running, [0, 12.5] meters per second, and cycling, [0, 100] meters per second. Default thresholds are converted to the units of measurment of the \code{object} before they are applied.
#' @examples
#' data('runs', package = 'trackeR')
#' plot(runs, session = 4, what = 'speed', threshold = FALSE)
#' runsT <- threshold(runs, variable = 'speed', lower = 0, upper = 12.5)
#' plot(runsT, session = 4, what = 'speed', threshold = FALSE)
#' @export
threshold <- function(object, variable, lower, upper, ...) {

    sports <- sport(object)

    ## if variable is NULL, just update attribute, leave data unchanged
    if (!missing(variable) && is.null(variable)) {
        operations <- getOperations(object)
        operations$threshold <- NULL
        attr(object, "operations") <- operations
        return(object)
    }

    ## prep default thresholds if nothing is specified
    if (missing(variable) & missing(lower) & missing(upper)) {
        units <- getUnits(object)
        th <- generateDefaultThresholds()
        th <- changeUnits(th, variable = units$variable, unit = units$unit)
        running_th <- changeUnits(running_th, variable = units$variable, unit = units$unit)
        swimming_th <- changeUnits(swimming_th, variable = units$variable, unit = units$unit)
    }
    else {
        ## new thresholds
        if (!missing(variable) && is.data.frame(variable)) {
            running_th <- cycling_th <- swimming_th <- variable
        }
        else {
            running_th <- cycling_th <- swimming_th <-
                data.frame(variable = variable, lower = lower, upper = upper)
        }
    }

    ## compare with existing thresholds
    operations <- getOperations(object)
    if (!is.null(operations$threshold)) {
        th <- merge(th, operations$threshold, by = "variable", all = TRUE)
        th$lower <- apply(th[, c("lower.x", "lower.y")], 1, max, na.rm = TRUE)
        th$upper <- apply(th[, c("upper.x", "upper.y")], 1, min, na.rm = TRUE)
    }

    ## apply thresholds
    for (i in 1:nrow(th)) {
        v <- as.character(th$variable[i])
        for (session in seq_along(object)) {
            if (v %in% names(object[[session]])) {
                wL <- which(object[[session]][, v] < th$lower[i])
                object[[session]][wL, v] <- NA  ## th$lower[i] ## set to boundary value or to NA?
                wU <- which(object[[session]][, v] > th$upper[i])
                object[[session]][wU, v] <- NA  ## th$upper[i]
            }
        }
    }

    ## update attribute
    operations$threshold <- th[, c("variable", "lower", "upper")]
    attr(object, "operations") <- operations

    return(object)
}


generateDefaultThresholds <- function(...) {
    th <- generateBaseUnits()
    n_variables <- nrow(th)
    th <- rbind(th, th, th)
    ## FIXME: tighter limits?
    th$lower <- c(-90, -180, -500, 0, 0, 0, 0, 0, 0, -30, 0, 0)
    th$sport <- rep(c("cycling", "running", "swimming"), each = n_variables)
    th$upper <- c(c(90, 180, 9000, Inf, 250, 100, Inf, Inf, Inf, 60, Inf, Inf),
                  c(90, 180, 9000, Inf, 250, 12.5, Inf, Inf, Inf, 60, Inf, Inf),
                  c(90, 180, 9000, Inf, 250, 5, Inf, Inf, Inf, 60, Inf, Inf))
    class(th) <- c("trackeRthresholds", class(th))
    return(th)
}

