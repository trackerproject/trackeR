#' Extract resting period characteristics.
#'
#' @param times Timestamps.
#' @param session_threshold The threshold in hours for the time
#'     difference between consecutive timestamps above which they are
#'     considered to belong to different training sessions.
#' @return A list containing a dataframe with start, end, and duration
#'     for each session and the resting time between sessions, named
#'     'sessions' and 'restingTime', respectively.
#' @export
get_resting_periods <- function(times, session_threshold) {
    if (length(times) == 0)
        return(NULL)
    t1 <- times[-length(times)]
    t2 <- times[-1]
    hoursBetweenObservations <- difftime(t2, t1, units = "hours")
    sessionEnd <- c(which(hoursBetweenObservations > session_threshold), length(times))
    sessionStart <- c(1, sessionEnd[-length(sessionEnd)] + 1)
    start <- times[sessionStart]
    ending <- times[sessionEnd]
    sessions <- data.frame(sessionStart = start,
                           sessionEnd = ending,
                           trainingDuration = difftime(ending, start, units = "hours"))
    resting <- difftime(start[-1], ending[-length(ending)], units = "hours")
    list(sessions = sessions, restingTime = resting)
}


## Detects sessions in the output of readX functions according to
## session_threshold and returns a multivariate zoo object
## session_threshold in hours!
get_sessions <- function(dat, session_threshold = 2) {
    ## get session IDs
    dat$sessionID <- NA
    resting <- get_resting_periods(dat$time, session_threshold)

    n_sessions <- nrow(resting$sessions)
    for (i in seq.int(n_sessions)) {
        session <- resting$sessions[i, 1:2]
        dat$sessionID[is_in_period(dat$time, start = session[[1]], end = session[[2]])] <- i
    }
    rownames(dat) <- NULL

    ## construct a multivariate zoo object for each session
    sessions <- unique(dat$sessionID)
    trackerdat <- vector("list", length = max(sessions))
    for (i in sessions) {
        dati <- subset(dat, dat$sessionID == i)
        extra <- which(names(dati) %in% c("time", "sessionID"))
        trackerdat[[i]] <- zoo(dati[, -extra], order.by = dati$time)
    }

    ## remove empty sessions
    trackerdat <- trackerdat[!sapply(trackerdat, is.null)]

    return(trackerdat)
}

