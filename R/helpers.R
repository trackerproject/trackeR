guess_sport <- function(sport) {
    keyword <- c("run", "hik", "cycl", "swim", "bik", "rid")
    sports <- c("running", "running", "cycling", "swimming", "cycling", "cycling")
    sport <- sports[sapply(keyword, function(key) grepl(key, sport, ignore.case = TRUE))]
    if (length(sport) == 0) {
        NA
    }
    else {
        sport
    }
}

removeColon <- function(x){
    sapply(strsplit(x, split = ":"), paste, collapse = "")
}

convertTCXTimes2POSIXct <- function(x, timezone = ""){

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
is_in_period <- function(dates, start, end) {
    (dates >= start) & (dates <= end)
}
