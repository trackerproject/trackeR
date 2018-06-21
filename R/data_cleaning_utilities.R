#' Extract resting period characteristics.
#'
#' @param times Timestamps.
#' @param sessionThreshold The threshold in hours for the time
#'     difference between consecutive timestamps above which they are
#'     considered to belong to different training sessions.
#' @return A list containing a dataframe with start, end, and duration
#'     for each session and the resting time between sessions, named
#'     'sessions' and 'restingTime', respectively.
#' @export
resting_periods <- function(times, sessionThreshold) {
    if (length(times) == 0)
        return(NULL)
    t1 <- times[-length(times)]
    t2 <- times[-1]
    hoursBetweenObservations <- difftime(t2, t1, units = "hours")
    ##
    sessionEnd <- c(which(hoursBetweenObservations > sessionThreshold), length(times))
    sessionStart <- c(1, sessionEnd[-length(sessionEnd)] + 1)
    start <- times[sessionStart]
    ending <- times[sessionEnd]
    sessions <- data.frame(sessionStart = start, sessionEnd = ending, trainingDuration = difftime(ending,
        start, units = "hours"))
    resting <- difftime(start[-1], ending[-length(ending)], units = "hours")
    list(sessions = sessions, restingTime = resting)
}


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
