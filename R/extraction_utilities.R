## Is the date within a certain period (including both start and end)?  Output is a
## logical vector for all dates.
in_period <- function(dates, start, end) {
    (dates >= start) & (dates <= end)
}

## Detects sessions in the output of readX functions according to
## session_threshold and returns a multivariate zoo object
get_sessions <- function(dat, sessionThreshold = 2) {
    ## get session IDs
    dat$sessionID <- NA
    resting <- resting_periods(dat$time, sessionThreshold)

    n_sessions <- nrow(resting$sessions)
    for (i in seq.int(n_sessions)) {
        session <- resting$sessions[i, 1:2]
        dat$sessionID[in_period(dat$time, start = session[[1]], end = session[[2]])] <- i
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
