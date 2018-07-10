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

find_unit_reference_sport <- function(object) {
    names(which.max(table(get_sport(object))))
}

## Collects the units from a reference sport and returns a simple
## unit-specification df.
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
#' @param a The level at which quantiles will be computed are \code{a}
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
        sess <- as.data.frame(sess)
        all_na <- apply(sess, 2, function(x) all(is.na(x)))
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


#' Time spent above a certain threshold.
#'
#' @param object A (univariate) zoo object.
#' @param threshold The threshold.
#' @param ge Logical. Should time include the thereshold (greater or equal to threshold) or not (greater only)?
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


