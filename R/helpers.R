guess_sport <- function(sport) {
    keyword <- c("run", "hik", "cycl", "swim", "bik", "rid")
    sports <- c("running", "running", "cycling", "swimming", "cycling", "cycling")
    sport <- sports[sapply(keyword, function(key) grepl(key, sport, ignore.case = TRUE))]
    if (length(sport) == 0) {
        NA
    }
    else {
        ## In case of many matches, return the sport from the first match only
        sport[1]
    }
}

#' Find the most frequent sport in an \code{object}
#'
#' @param object any object with a \code{\link{get_sport}} method
#'     implemented (run \code{methods(get_sport)}).
#'
#' @export
find_unit_reference_sport <- function(object) {
    names(which.max(table(get_sport(object))))
}

#' Collect units from the result of \code{\link{generate_units}}
#'
#' Collects the units from the results of \code{\link{generate_units}}
#' according to a \code{unit_reference_sport}
#'
#' @param object a \code{data.frame}, as returned by
#'     \code{\link{generate_units}}
#' @param unit_reference_sport The sport to inherit units from
#'     (default is taken to be the most frequent sport in
#'     \code{object}).
#' @export
collect_units <- function(object,
                          unit_reference_sport = NULL) {

    ## Match units to those of unit_reference_sport
    unit_reference_sport <- match.arg(unit_reference_sport, c("cycling", "running", "swimming"))

    units <- object[object$sport == unit_reference_sport, ]
    ## Add missing variables
    units <- rbind(units, object[!(object$variable %in% units$variable), ])
    units$sport <- NULL
    rownames(units) <- NULL
    attr(units, "unit_reference_sport") <- unit_reference_sport
    units
}


removeColon <- function(x) {
    sapply(strsplit(x, split = ":"), paste, collapse = "")
}

convertTCXTimes2POSIXct <- function(x,
                                    timezone = ""){

    ## get first non-NA element to determine the format
    formatSample <- x[which.min(is.na(x))]

    ## set basis for format
    frm <- "%Y-%m-%dT%H:%M:"

    if (nchar(formatSample) <= 19L) {
        ## just 2 characters for the seconds, nothing else
        frm <- paste0(frm, "%S")
    }
    else {

        rest <- substr(formatSample, start = 20, stop = nchar(formatSample))

        if (substr(rest, 1, 1) %in% c(".", ",")) {
            rest <- substr(rest, start = 2, stop = nchar(rest))

            ## determine the number of digits for the seconds
            splitted <- strsplit(rest, split = "")[[1]]
            ndigits <- which.min(splitted %in% c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")) - 1

            ## remove any digits beyond 6
            if (ndigits > 6){
                x <- paste0(substr(x, 1, 26), substr(x, 20 + ndigits + 1, nchar(formatSample)))
            }

            ## update format
            frm <- paste0(frm, "%OS")#, min(ndigits, 6))

            ## get remainder beyond seconds for timezone specification
            rest <- substr(rest, ndigits + 1, nchar(rest))

        }
        else {
            ## add seconds to format
            frm <- paste0(frm, "%S")
            ndigits <- 0
        }

        ## work with remainder to check for timezone specification

        if (rest != ""){
            if (substr(rest, 1, 1)  == "Z"){
                if (!(timezone %in% c("GMT", "UCT")) & timezone != "")
                    warning("Time zone will be UTC as recorded in the TCX file.")
                timezone <- "UTC"
                ##x <- substr(x, start = 1, stop = 19)
                ##frm <- "%Y-%m-%dT%H:%M:%S"
            }
            if (substr(rest, 1, 1) %in% c("-", "+")) { ## include hyphen?
                base <- 19 + ifelse(ndigits < 1, 0, min(ndigits, 6) + 1) ## +1 corresponds to "."
                x <- paste0(substr(x, start = 1, stop = base),
                            removeColon(substr(x, base + 1, nchar(formatSample))))
                frm <- paste0(frm, "%z")
            }
        }
    }

    as.POSIXct(x, format = frm, tz = timezone)
}

## Is the date within a certain period (including both start and end)?  Output is a
## logical vector for all dates.
is_in_period <- function(dates,
                         start,
                         end) {
    (dates >= start) & (dates <= end)
}

## Produce clean grids potentially exceeding the maximum
clean_grid <- function (minimum, maximum) {
    if (is.na(minimum) | is.na(maximum)) {
        return(NULL)
    }
    value_range <- as.character(ceiling(maximum - minimum))
    range_size <- nchar(value_range)
    round_table <- list('1' = 5,
                        '2' = 5,
                        '3' = 1e+01,
                        '4' = 1e+02,
                        '5' = 1e+03,
                        '6' = 1e+04,
                        '7' = 1e+05,
                        '8' = 1e+06)
    minimum <- floor(minimum/round_table[[range_size]]) * round_table[[range_size]]
    maximum <- ceiling(maximum/round_table[[range_size]]) * round_table[[range_size]]
    break_points <- seq(minimum, maximum, by = (maximum - minimum) / 200)
    break_points
}

#' Compute variable limits from a \code{\link{trackeRdata}} object.
#'
#' @param object A \code{\link{trackeRdata}} object.
#' @param a The levels at which quantiles will be computed are \code{a}
#'     and \code{1 - a}. Default is \code{a = 0.0001}.
#'
#' @details
#'
#' \code{compute_limits} computes limits by finding the \code{a} and
#' \code{1 - a} quantiles for each variable in each session, and then
#' taking the minimum and maximum of the \code{a} and \code{1 - a},
#' respectively, across sessions.
#'
#' @export
compute_limits <- function(object, a = 0.0001) {
    limits <- lapply(object, function(sess) {
        sess <- coredata(sess)
        apply(sess, 2, quantile, probs = c(a, 1 - a), na.rm = TRUE)

    })

    low <- apply(sapply(limits, function(x) x[1, ]), 1, function(x) if (all(is.na(x))) NA else min(x, na.rm = TRUE))
    upp <- apply(sapply(limits, function(x) x[2, ]), 1, function(x) if (all(is.na(x))) NA else max(x, na.rm = TRUE))
    inds <- low == upp
    low[inds] <- upp[inds] <- NA
    out <- lapply(seq.int(length(low)), function(v) {
        unname(c(low[v], upp[v]))
    })
    names(out) <- names(low)
    out
}


#' Compute a grid of breakpoints per variable from a \code{\link{trackeRdata}} object.
#'
#' @param object A \code{\link{trackeRdata}} object.
#' @param a The levels at which quantiles will be computed are
#'     \code{a} and \code{1 - a}. Default is \code{a = 0.0001}.
#' @param n_breaks A scalar determining the number of breakpoints to
#'     be computed
#' @param limits A list of a vectors, each specifying the lower and
#'     upper limit for each variable to be used when computing the
#'     grid. Default is \code{NULL}, in which case
#'     \code{\link{compute_limits}} is used.
#' @param what The variables for which a grid of breakpoints should be
#'     computed. Defaults to \code{c("speed", "heart_rate")}.
#' @return
#'
#' A named list with names as in \code{what}, with elements the grids
#' of breakpoints per variable.
#'
#' @examples
#' data("runs")
#' compute_breaks(runs, what = c("speed", "heart_rate", "altitude"))
#' @export
compute_breaks  <- function(object,
                            a = 0.0001,
                            n_breaks = 9,
                            limits = NULL,
                            what = c("speed", "heart_rate")) {
    breaks <- NULL
    if (is.null(limits)) {
        limits <- compute_limits(object, a = a)
    }
    for (feature in what) {
        if (all(is.na(limits[[feature]]))) {
            ## warning(paste('no data for', feature))
            what <- what[!(what %in% feature)]
            limits[[feature]] <- NULL
        }
    }
    break_points <- function(maximum, minimum = 0) {
        value_range <- as.character(ceiling(maximum - minimum))
        range_size <- nchar(value_range)
        round_table <- list('1' = 5, '2' = 5, '3' = 10, '4' = 100,
                            '5' = 10000, '6' = 100000)
        maximum <- ceiling(maximum/round_table[[range_size]]) * round_table[[range_size]]
        step_size <- round((maximum - minimum) / (n_breaks), 1)
        break_points <- seq(minimum, minimum + n_breaks * step_size, by = step_size)
        break_points
    }
    for (feature in what) {
        maximum <- ceiling(limits[[feature]][2])
        minimum <- floor(limits[[feature]][1])
        breaks[[feature]] <- break_points(maximum, minimum)
    }
    breaks
}


#' Time spent above a certain threshold.
#'
#' @param object A (univariate) zoo object.
#' @param threshold The threshold.
#' @param ge Logical. Should time include the threshold (greater or equal to threshold) or not (greater only)?
timeAboveThreshold <- function(object, threshold = -1, ge = TRUE) {
    n <- length(object)
    if (ge){
        aboveThreshold <- object >= threshold
    } else {
        aboveThreshold <- object > threshold
    }
    missing <- is.na(object)
    dt <- diff(index(object))
    sum(dt[aboveThreshold[-n] & !missing[-n]])
}

#' (Cumulative) Elevation gain.
#'
#' @param object A (univariate) zoo object.
#' @param smooth_elevation_gain Logical. Should the elevation be
#'     smoothed? Default is \code{TRUE}.
#' @param cumulative Logical. Return the cumulative elevation gain
#'     (\code{FALSE}; default) or just the elevation gain?
get_elevation_gain <- function(object, smooth = FALSE, cumulative = FALSE) {
    eg <- c(0, diff(object$altitude))
    if (smooth) {
        eg <- predict(smooth.spline(index(object), eg))$y
    }
    if (cumulative) {
        eg[eg < 0] <- 0
        cumsum(eg)
    }
    else {
        eg
    }
}
