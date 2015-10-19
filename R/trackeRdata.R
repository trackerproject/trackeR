#' Create a trackeRdata object.
#'
#' Create a trackeRdata object from a data frame with observations being divided in
#' separate training sessions. For breaks within a session observations are imputed. 
#' 
#' @param dat A data frame.
#' @param units A data frame containing the unit of measurement for all variables. See Details.
#' @inheritParams restingPeriods
#' @inheritParams imputeSpeeds
#' @details The \code{units} argument takes a data frame with two variables named \code{variable} and \code{unit}.
#'     Possible options include:
#'     \itemize{
#'     \item variables \code{latitude} and \code{longitude} with unit \code{degree}
#'     \item variables \code{altitude}, \code{distance} with unit \code{m}, \code{km}, \code{mi} or \code{ft}
#'     \item variable \code{heart.rate} with unit \code{bpm}
#'     \item variable \code{speed} with unit \code{m_per_s}, \code{km_per_h}, \code{ft_per_min},
#'           \code{ft_per_s} or \code{mi_per_h}
#'     \item variable \code{cadence} with unit \code{steps_per_min} or \code{rev_per_min}
#'     \item variable \code{power} with unit \code{W} or \code{kW}.
#'     }
#' 
#'     During small breaks within a session, e.g., because the recording device was paused,
#'     observations are imputed the following way: 
#'     0 for speed, last known position for latitude, longitude and altitude, and
#'     NA for all other variables. Distances are (re-)calculated based on speeds after imputation.
#' @seealso \code{\link{readContainer}} for reading .tcx and .db3 files directly into \code{trackeRdata} objects.
#' @examples
#' \dontrun{
#' ## read raw data
#' filepath <- system.file("extdata", "2013-06-04-174137.TCX", package = "trackeR")
#' run <- readTCX(file = filepath, timezone = "GMT")
#'
#' ## turn into trackeRdata object
#' run <- trackeRdata(run, units = data.frame(variable = c("latitude", "longitude", 
#'     "altitude", "distance", "heart.rate", "speed", "cadence", "power"),
#'     unit = c("degree", "degree", "m", "m", "bpm", "m_per_s", "steps_per_min", "W"),
#'     stringsAsFactors = FALSE))
#'
#' ## alternatively
#' run <- readContainer(filepath, type = "tcx", timezone = "GMT")
#' }
#' @export
trackeRdata <- function(dat, units = NULL, sessionThreshold = 2,
                    fromDistances = TRUE, lgap = 30, lskip = 5, m = 11){
    ## basic edits on time stamps
    dat <- basicEdits(dat, sessionThreshold = sessionThreshold)
    
    ## impute (calculated) speeds, cast to zoo
    trackerdat <- dataHandling(dat, fromDistances = fromDistances, lgap = lgap, lskip = lskip, m = m)

    ## Set operations attribute
    attr(trackerdat, "operations") <- list(smooth = NULL, threshold = NULL)

    ## Set units attribute
    if (is.null(units)) {
        allnames <- generateVariableNames()$humanNames
        units <- data.frame(variable = allnames[-which(allnames == "time")] ,
                            unit = c("degree", "degree", "m", "m", "bpm", "m_per_s", "steps_per_min", "w"),
                            stringsAsFactors = FALSE)
    }
    for (i in seq_len(ncol(units))) {
        if (is.factor(units[,i])) units[,i] <- as.character(units[,i])
    }
    attr(trackerdat, "units") <- units

    ## class and return
    class(trackerdat) <- c("trackeRdata", class(trackerdat))
    return(trackerdat)
}


#' Extract resting period characteristics.
#'
#' @param times Timestamps.
#' @param sessionThreshold The threshold in hours for the time
#' difference between consecutive timestamps above which they are
#' considered to belong to different training sessions.
#' @return A list containing a dataframe with start, end, and duration
#' for each session and the resting time between sessions, named
#' "sessions" and "restingTime", respectively.
#' @export
restingPeriods <- function(times, sessionThreshold) {
    if (length(times) == 0) return(NULL)
    t1 <- times[-length(times)]
    t2 <- times[-1]
    hoursBetweenObservations <- difftime(t2, t1, units = "hours")
    ##
    sessionEnd <- c(which(hoursBetweenObservations > sessionThreshold),
                    length(times))
    sessionStart <- c(1, sessionEnd[-length(sessionEnd)] + 1)
    start <- times[sessionStart]
    ending <- times[sessionEnd]
    sessions <- data.frame(sessionStart = start,
                           sessionEnd = ending,
                           trainingDuration = difftime(ending, start, units = "hours"))
    resting <- difftime(start[-1], ending[-length(ending)], units = "hours")
    list(sessions = sessions, restingTime = resting)
}

## Is the date within a certain period (including both start and end)?
## Output is a logical vector for all dates.
inPeriod <- function(dates, start, end) {
    (dates >= start) & (dates <=end)
}


basicEdits <- function(dat, sessionThreshold = 2){
    ## replace heart rate 0 with NA
    dat$heart.rate[dat$heart.rate == 0] <- NA

    ## handle NAs
    natime <- is.na(dat$time)
    if (all(natime))
        stop("The are no useable timestamps.")
    dat <- dat[!natime, ]

    ## remove duplicates
    dat <- dat[!duplicated(dat$time),]

    ## order according to time
    dat <- dat[order(dat$time), ]

    ## get session IDs
    dat$sessionID <- NA
    resting <- restingPeriods(dat$time, sessionThreshold)
    nSessions <- nrow(resting$sessions)
    for (i in seq.int(nSessions)){
        session <- resting$sessions[i, 1:2]
        dat$sessionID[inPeriod(dat$time, start = session[[1]], end = session[[2]])] <- i
    }

    rownames(dat) <- NULL
    return(dat)
}


dataHandling <- function(dat, fromDistances = TRUE, lgap = 30, lskip = 5, m = 11){

    ## make multivariate zoo object for each session
    sessions <- unique(dat$sessionID)
    trackeRdata <- vector("list", length = max(sessions))
    for (i in sessions){
        dati <- subset(dat, dat$sessionID == i)
        extra <- which(names(dati) %in% c("time", "sessionID"))
        trackeRdata[[i]] <- zoo(dati[, -extra], order.by = dati$time)
    }
    ## remove empty sessions
    trackeRdata <- trackeRdata[!sapply(trackeRdata, is.null)]

    ## impute speeds in each session
    trackeRdata <- lapply(trackeRdata, imputeSpeeds, fromDistances = fromDistances,
                     lgap = lgap, lskip = lskip, m = m)

    return(trackeRdata)
}


#' @export
c.trackeRdata <- function(..., recursive = FALSE){
    ## FIXME: recursive argument

    input <- list(...)
    ninput <- length(input)
    if (ninput < 2) return(input[[1]])

    nsessionsInput <- sapply(input, length)
    units1 <- getUnits(input[[1]])
    operations <- getOperations(input[[1]])

    ## check/change operations attributes: smooth

    ## if all smoother settings are NULL, skip whole aggregation process
    if (!all(sapply(input, function(x) is.null(getOperations(x)$smooth)))) {
    
        ## if the settings for the first session are NULL, create a new reference setup
        if (is.null(getOperations(input[[1]])$smooth)){
            operations$smooth <- list(fun = NA, width = NA,
                                     parallel = TRUE, mc.cores = getOption("mc.cores", 2L),
                                     what = NA, nsessions = NULL)
        }

        
        funs <- sapply(input, function(x) getOperations(x)$smooth$fun)
        funs <- funs[!sapply(funs, is.null)]
        funs <- funs[!sapply(funs, is.na)]
        if(any(!sapply(funs, function(x) isTRUE(all.equal(funs[[1]], x))))) 
            stop("Smoothing function must be the same for all sessions.")
        if (is.na(operations$smooth$fun)) operations$smooth$fun <- funs[[1]]
    
        widths <- lapply(input, function(x) unique(getOperations(x)$smooth$width))
        whats <- lapply(input, function(x) unique(getOperations(x)$smooth$what))
        changeWidth <- any(!sapply(widths, function(x) isTRUE(all.equal(widths[[1]], x))))
        changeWhat <- any(!sapply(whats, function(x) isTRUE(all.equal(whats[[1]], x))))
        changeO <- changeWidth | changeWhat
        if (changeO) {
            widths <- lapply(input, function(x) getOperations(x)$smooth$width)
            widths[sapply(widths, is.null)] <- operations$smooth$width[1]
            widths <- do.call("c", widths)
            whats <- lapply(input, function(x) getOperations(x)$smooth$what)
            whats[sapply(whats, is.null)] <- list(operations$smooth$what[1])
            whats <- do.call("c", whats)
            nsessions <- lapply(input, function(x) getOperations(x)$smooth$nsessions)
            nsessions[sapply(nsessions, is.null)] <- nsessionsInput[sapply(nsessions, is.null)]
            nsessions <- do.call("c", nsessions)

            operations$smooth$width <- widths
            operations$smooth$what <- whats
            operations$smooth$nsessions <- nsessions
        } else {
            nsessions <- lapply(input, function(x) getOperations(x)$smooth$nsessions)
            nsessions[sapply(nsessions, is.null)] <- nsessionsInput[sapply(nsessions, is.null)]
            operations$smooth$nsessions <- sum(do.call("c", nsessions))
        }
    }

    ## check/change operations attributes: threshold
    ## apply thresholds of first session to all sessions if necessary
    th <- operations$threshold
    thAll <- lapply(input, function(x) getOperations(x)$threshold)
    changeT <- !all(sapply(thAll, function(x) isTRUE(all.equal(th, x))))
    if(changeT) {
        if (is.null(th)) {
            warning("The first session does not have any thresholds, this is applied to all sessions.")
        } else {
            warning("The sessions have different thresholds. The thresholds of the first session are applied to all sessions.")
        }
        ## change thresholds
        for (i in 2:ninput){
            input[[i]] <- threshold(input[[i]], th)
        }
    }
        
    ## check/change units attribute
    units <- lapply(input, getUnits)
    changeU <- !all(sapply(units, function(x) isTRUE(all.equal(units[[1]], x))))
    if(changeU) {
        warning("The sessions have different units. The units of the first session are applied to all sessions.")
        ## change units 
        for (i in 2:ninput){
            input[[i]] <- changeUnits(input[[i]], variable = units1$variable, unit = units1$unit)
        }
    }

    ## combine sessions
    ret <- vector("list", sum(nsessionsInput))
    starti <- c(1, cumsum(nsessionsInput)[-length(nsessionsInput)] + 1)
    endi <- cumsum(nsessionsInput)
    for (i in seq_len(ninput)){
        ret[starti[i]:endi[i]] <- input[[i]]
    }

    ## class and other attributes
    class(ret) <- c("trackeRdata", "list")
    attr(ret, "units") <- units1
    ## operations$smooth
    attr(ret, "operations") <- operations

    return(ret)

}

"[.trackeRdata" <- function(x, i, j, drop = TRUE, ...){

    units <- getUnits(x)
    operations <- getOperations(x)

    ##ret <- x[i]
    ret <- NextMethod()

    if(!is.null(operations$smooth)){
        smooth <- operations$smooth
        ## select right smoothing parameters for the i session(s)

        ## elements j from smooting settings
        j <- rep(seq_along(smooth$nsessions), times = smooth$nsessions)[i]

        if(length(j) < 2){
            k <- j
            nsessions <- length(j)
        } else {
            ## to avoid duplicating unnecessary information, aggregate j to k and keep track of number of sessions
            ## NOTE:
            ## k <- unique(j) ; smooth$nsessions <- as.numeric(table(j))
            ## does not allow to split sessions from one block - but x[i] does allow it.
            ## Thus the following aggregation to k and nsessions:        
            counter <- breakpoints <- rep(NA, length(j))
            counter[1] <- 1
            breakpoints[1] <- TRUE
            for (a in 2:length(j)){
                if (j[a] == j[a-1]) {
                    counter[a] <- counter[a-1] + 1
                    breakpoints[a] <- FALSE
                } else {
                    counter[a] <- 1
                    breakpoints[a] <- TRUE
                }
            }
            ##cbind(j, counter, breakpoints)
            k <- j[breakpoints]
            nsessions <- counter[c(which(breakpoints)[-1] - 1, length(j))]
        }
        
        smooth$width <- smooth$width[k]
        smooth$what <- smooth$what[k]
        smooth$nsessions <- nsessions
        operations$smooth <- smooth
    }

    ## class and attributes
    class(ret) <- c("trackeRdata", "list")
    attr(ret, "units") <- units
    attr(ret, "operations") <- operations
    
    return(ret)
}


#' Append training sessions to existing file.
#'
#' @param object The object to be appended.
#' @param file The file to which \code{object} is to be appended.
#' @param ... Currently not used.
#' @export
append.trackeRdata <- function(object, file, ...){
    old <- load(file)
    new <- c(old, object)
    save(new, file)
}
