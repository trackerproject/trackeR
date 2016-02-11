#' Generate training distribution profiles.
#'
#' @param object An object of class \code{\link{trackeRdata}}.
#' @param session A numeric vector of the sessions to be used, defaults to all sessions.
#' @param what The variables for which the distribution profiles should be generated.
#' @param grid A named list containing the grid for the variables in \code{what}.
#' @param scaled Logical. Should the distribution profiles be scaled relative to their maximum value?
#' @param parallel Logical. Should computation be carried out in parallel?
#' @param cores Number of cores for parallel computing. If NULL, the number of cores is set to the value of \code{options("corese")} (on Windows) or \code{options("mc.cores")} (elsewhere), or, if the relevant option is unspecified, to half the number of cores detected.
#' @return An object of class \code{distrProfile}.
#' @references Kosmidis, I., and Passfield, L. (2015). Linking the Performance of
#'     Endurance Runners to Training and Physiological Effects via Multi-Resolution
#'     Elastic Net. \emph{ArXiv e-print} arXiv:1506.01388.
#' @examples
#' data(run, package = "trackeR")
#' dProfile <- distributionProfile(run, what = "speed", grid = seq(0, 12.5, by = 0.05))
#' plot(dProfile, smooth = FALSE)
#' @export
distributionProfile <- function(object, session = NULL, what = c("speed", "heart.rate"),
                                grid = list(speed = seq(0, 12.5, by = 0.05), heart.rate = seq(0, 250)),
                                scaled = FALSE, parallel = FALSE, cores = NULL){ 
    
    units <- getUnits(object)
    if (is.null(session))
        session <- 1:length(object)
    object <- object[session]

    ## check supplied args
    ## if it's a list, it has to either has to be named and contain all element in what or
    ## has to have the same length as what, then it's assumed that the order is the same.
    if (is.list(grid)) {
        if (is.null(names(grid)) & length(what) == length(grid))
            names(grid) <- what
        if (is.null(names(grid)))
            stop("Can't match variables in argument 'what' and their grid. Please provide a named list.")
        if (any(is.na(match(what, names(grid)))))
            stop("Please provide a grid for all variables in argument 'what'.")
        grid <- grid[what]
    }
    else {
        if (length(what) == 1L & is.vector(grid)) {
            grid <- list(grid)
            names(grid) <- what
        }
        else {
            stop("Arguments 'what' and 'grid' don't match.")
        }
    }

    ## README: original checks
    #stopifnot(all.equal(length(what), length(grid)))
    #if (is.null(names(grid))) names(grid) <- what
    stopifnot(!any(is.na(match(what, names(grid)))))

    ## FIXME: transform grid if units of trackeRdata object are not m/s and bpm for speed and hr, respectively

    dp <- function(object, grid) {
        values <- coredata(object)
        timestamps <- index(object)
        ## Remove NA
        missing <- is.na(values)
        values <- values[!missing]
        timestamps <- timestamps[!missing]
        ## Calculate dp
        charOrder <- order(values)
        values <- values[charOrder]
        uniqueValues <- unique(values)
        weights <- c(0, difftime(timestamps[-1], timestamps[-length(timestamps)], units = "secs"))[charOrder]
        weights <- sapply(uniqueValues, function(xx) sum(weights[values == xx]))
        out <- stats::approx(x = uniqueValues, y = cumsum(weights)/sum(weights), xout = grid,
                      method = "constant", yleft = 0, yright = 1, f = 0, ties = "ordered")$y
        ## Can be included for scaling
        if (scaled)
            1 - out
        else
        sum(weights)*(1 - out)
    }

    ## get distribution profile
    DP <- list()
    ## all profiles (for all sessions) of the same variable are collected in a multivariate zoo object
    ## - and not all profiles per session because the profiles might have a differently lengthed grid.
    if (parallel) {
        dc <- parallel::detectCores()
        if (.Platform$OS.type != "windows"){
            if (is.null(cores))
                cores <- getOption("mc.cores", max(floor(dc/2), 1L)) 
            ## parallel::mclapply(...,  mc.cores = cores)
            for (i in what) {
                times <- parallel::mclapply(object, function(sess) {
                    dp(sess[, i], grid[[i]])
                }, mc.cores = cores)
                times <- zoo(matrix(unlist(times), nrow = length(grid[[i]])),
                             order.by = grid[[i]])
                names(times) <- paste0("Session", session)
                DP[[i]] <- times
            }
        } else {
            if (is.null(cores))
                cores <- getOption("cores", max(floor(dc/2), 1L))
            cl <- parallel::makePSOCKcluster(rep("localhost", cores))
            ## parallel::parLapply(cl, ...)
            for (i in what) {
                times <- parallel::parLapply(cl, object, function(sess) {
                    dp(sess[, i], grid[[i]])
                })
                times <- zoo(matrix(unlist(times), nrow = length(grid[[i]])),
                             order.by = grid[[i]])
                names(times) <- paste0("Session", session)
                DP[[i]] <- times
            }
            parallel::stopCluster(cl)
        }
    } else {
        ## lapply(...)
        for (i in what) {
            times <- lapply(object, function(sess) {
                dp(sess[, i], grid[[i]])
            })
            times <- zoo(matrix(unlist(times), nrow = length(grid[[i]])),
                         order.by = grid[[i]])
            names(times) <- paste0("Session", session)
            DP[[i]] <- times
        }
    }

    operations <- list(smooth = NULL)
    attr(DP, "operations") <- operations
    attr(DP, "units") <- units
    class(DP) <- "distrProfile"
    return(DP)
}



#' Time spent above a certain threshold
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

    ## indices <- seq_along(object)
    ## times <- index(object)
    ## if (ge)
    ##     indicesAboveThreshold <- indices[object >= threshold & !is.na(object >= threshold)]
    ## else
    ##     indicesAboveThreshold <- indices[object > threshold & !is.na(object > threshold)]
    ## timesAboveThreshold <- times[indicesAboveThreshold]
    ## n <- length(indicesAboveThreshold)
    ## sum(diff(timesAboveThreshold)[diff(indicesAboveThreshold) == 1])

}


#' Fortify a distrProfile object for plotting with ggplot2.
#'
#' @param model The \code{distrProfile} object.
#' @param data Ignored.
#' @param melt Logical. Should the data be melted into long format
#'     instead of the default wide format?
#' @param ... Ignored.
#' @export
fortify.distrProfile <- function(model, data, melt = FALSE, ...){
    ret <- list()
    for (i in seq_along(model)){

        ret[[i]] <- zoo::fortify.zoo(model[[i]], melt = melt)
        ret[[i]]$Profile <- names(model)[i]
    }
    ret <- do.call("rbind", ret)
    return(ret)
}


#' Plot distribution profiles.
#'
#' @param x An object of class \code{distrProfile} as returned by \code{\link{distributionProfile}}.
#' @param session A numeric vector of the sessions to be plotted, defaults to all sessions.
#' @param what Which variables should be plotted?
#' @param multiple Logical. Should all sessions be plotted in one panel?
#' @param smooth Logical. Should unsmoothed profiles be smoothed before plotting?
#' @param ... Further arguments to be passed to \code{\link{smootherControl.distrProfile}}.
#' @examples
#' data(runs, package = "trackeR")
#' dProfile <- distributionProfile(runs, session = 1:2,
#'     what = "speed", grid = seq(0, 12.5, by = 0.05))
#' plot(dProfile, smooth = FALSE)
#' plot(dProfile, smooth = FALSE, multiple = TRUE)
#' plot(dProfile, multiple = TRUE)
#' @export
plot.distrProfile <- function(x, session = NULL, what = c("speed", "heart.rate"),
                            multiple = FALSE, smooth = TRUE, ...){
    ## code inspired by autoplot.zoo
    units <- getUnits(x)
    operations <- getOperations(x)

    ## select variables
    what <- what[what %in% names(x)]
    x <- x[what] ## FIXME: implement [] method for profiles/variables instead of sessions
    class(x) <- "distrProfile"; attr(x, "operations") <- operations; attr(x, "unit") <- units

    ## select sessions
    availSessions <- if (is.null(ncol(x[[1]]))) 1 else ncol(x[[1]])
    if (is.null(session)) session <- 1:availSessions
    for(i in what) x[[i]] <- x[[i]][,session]

    ## smooth
    if (smooth){
        if (!is.null(operations$smooth)){
            warning("This object has already been smoothed. No additional smoothing takes place.")
        } else {
            x <- smoother(x, what = what, ...)
        }
    }

    ## get data
    rownames(x) <- NULL
    df <- fortify(x, melt = TRUE)
    ## if (length(session) > 1L) df <- subset(df, Series %in% paste0("Session", session))
    ## df <- subset(df, Profile %in% what)
    ## HACK: If there is only one session (=series) to be plotted, give it a proper name for multiple = TRUE.
    if (length(session) < 2) df$Series <- paste0("Session", session)
    df$Series <- factor(df$Series)
    df$Profile <- factor(df$Profile)

    ## ## check that there is data to plot
    ## for(l in levels(df$Series)){
    ##     if (all(is.na(subset(df, Series == l, select = "Value"))))
    ##         df <- df[!(df$Series == l), ]
    ## }

    ## make basic plot and facets
    singleVariable <- nlevels(df$Profile) == 1L
    singleSession <- nlevels(df$Series) == 1L
    lab_data <- function(series){
        thisunit <- units$unit[units$variable == series]
        prettyUnit <- prettifyUnits(thisunit)
        paste0(series, " [", prettyUnit,"]")
    }
    lab_data <- Vectorize(lab_data)

    if (multiple){
        p <- ggplot2::ggplot(data = df, mapping = ggplot2::aes_(x = quote(Index), y = quote(Value),
                                                                group = quote(Series), color = quote(Series))) +
            ggplot2::geom_line() + ggplot2::ylab("Time spent above threshold") +
                ggplot2::xlab(if(singleVariable) lab_data(levels(df$Profile)) else "")
        facets <- if(singleVariable) NULL else ". ~ Profile"
    } else {
        p <- ggplot2::ggplot(data = df, mapping = ggplot2::aes_(x = quote(Index), y = quote(Value))) +
            ggplot2::geom_line() + ggplot2::ylab("Time spent above threshold") +
                ggplot2::xlab(if(singleVariable) lab_data(levels(df$Profile)) else "")

        facets <- if (singleVariable) {
            if (singleSession) NULL else "Series ~ ."
        } else {
            if(singleSession) ". ~ Profile" else "Series ~ Profile"
        }
    }

    ## add facets if necessary
    if (!is.null(facets)){
        p <- p + ggplot2::facet_grid(facets, scales = "free_x", labeller = ggplot2::labeller("Profile" = lab_data))
    }

    ## add bw theme
    p <- p + ggplot2::theme_bw()

    return(p)
}



## FIXME: example
#' Smoother for distribution profiles.
#'
#' The distribution profiles are smoothed using a shape constrained additive model with Poisson
#' responses to ensure that the smoothed distribution profile is positive and monotone decreasing.
#'
#' @param object An object of class \code{distrProfile} as returned by \code{\link{distributionProfile}}.
#' @param session A numeric vector of the sessions to be selected and smoothed. Defaults to all sessions.
#' @param control A list of parameters for controlling the smoothing process.
#'     This is passed to \code{\link{smootherControl.distrProfile}}.
#' @param ... Arguments to be used to form the default \code{control} argument if it is not supplied directly.
#' @seealso \code{\link{smootherControl.distrProfile}}
#' @references Kosmidis, I., and Passfield, L. (2015). Linking the Performance of
#'     Endurance Runners to Training and Physiological Effects via Multi-Resolution
#'     Elastic Net. \emph{ArXiv e-print} arXiv:1506.01388.
#'
#'     Pya, N. and Wood S. (2015). Shape Constrained Additive Models. Statistics and
#'     Computing, 25(3), 543--559.
#' @export
smoother.distrProfile <- function(object, session = NULL, control = list(...), ...){

    units <- getUnits(object)

    ## evaluate control argument
    control <- do.call("smootherControl.distrProfile", control)

    ## ## select variables
    ## object <- object[unlist(control$what)]
    ## class(object) <- "distrProfile" ## FIXME: implement [] method
    ## ## select sessions
    ## availSessions <- if (is.null(ncol(object[[1]]))) 1 else ncol(object[[1]])
    ## if (is.null(session)) session <- 1:availSessions
    ## for(i in unlist(control$what)) object[[i]] <- object[[i]][,session]
    ## #availSessions <- if (is.null(ncol(object[[1]]))) 1 else ncol(object[[1]])

    ## select sessions
    availSessions <- if (is.null(ncol(object[[1]]))) 1 else ncol(object[[1]])
    if (is.null(session)) session <- 1:availSessions
    for(i in seq_along(object)) object[[i]] <- object[[i]][,session]
    #availSessions <- if (is.null(ncol(object[[1]]))) 1 else ncol(object[[1]])


    ## smooth
    ret <- list()
    what <- unlist(control$what)[unlist(control$what) %in% names(object)]
    if (control$parallel) {
        if (.Platform$OS.type != "windows"){
            ## parallel::mclapply(...,  mc.cores = control$cores)
            for (i in what){
                nc <- if (is.null(ncol(object[[1]]))) 1 else ncol(object[[1]])
                smoothedProfile <- parallel::mclapply(seq_len(nc), function(j){
                    dsm <- decreasingSmoother(x = index(object[[i]]),
                                              y = coredata(object[[i]][,j]),
                                              k = control$k, len = NULL, sp = control$sp, fam = "poisson")
                    dsm$y
                }, mc.cores = control$cores)
                smoothedProfile <- do.call("cbind", smoothedProfile) ## is this a matrix if there is only one profile?
                colnames(smoothedProfile) <- attr(object[[i]], "dimnames")[[2]]
                ret[[i]] <- zoo(smoothedProfile, order.by = index(object[[i]]))
                ## README: originally used order.by = dsm$x which is the same as arg x of decreasingSmoother if len = NULL.
            }
        } else {
            cl <- parallel::makePSOCKcluster(rep("localhost", control$cores))
            ## parallel::parLapply(cl, ...)
            for (i in what){
                nc <- if (is.null(ncol(object[[1]]))) 1 else ncol(object[[1]])
                smoothedProfile <- parallel::parLapply(cl, seq_len(nc), function(j){
                    dsm <- decreasingSmoother(x = index(object[[i]]),
                                              y = coredata(object[[i]][,j]),
                                              k = control$k, len = NULL, sp = control$sp, fam = "poisson")
                    dsm$y
                })
                smoothedProfile <- do.call("cbind", smoothedProfile) ## is this a matrix if there is only one profile?
                colnames(smoothedProfile) <- attr(object[[i]], "dimnames")[[2]]
                ret[[i]] <- zoo(smoothedProfile, order.by = index(object[[i]]))
                ## README: originally used order.by = dsm$x which is the same as arg x of decreasingSmoother if len = NULL.
            }
            parallel::stopCluster(cl)
        }
    } else {
        ## lapply(...)
        for (i in what){
            nc <- if (is.null(ncol(object[[1]]))) 1 else ncol(object[[1]])
            smoothedProfile <- lapply(seq_len(nc), function(j){
                dsm <- decreasingSmoother(x = index(object[[i]]),
                                          y = coredata(object[[i]][,j]),
                                          k = control$k, len = NULL, sp = control$sp, fam = "poisson")
                dsm$y
            })
            smoothedProfile <- do.call("cbind", smoothedProfile) ## is this a matrix if there is only one profile?
            colnames(smoothedProfile) <- attr(object[[i]], "dimnames")[[2]]
            ret[[i]] <- zoo(smoothedProfile, order.by = index(object[[i]]))
            ## README: originally used order.by = dsm$x which is the same as arg x of decreasingSmoother if len = NULL.
        }
    }
   
    ## FIXME: add unsmoothed distribution profiles?
    unsmoothed <- names(object)[!(names(object) %in% what)]
    for (i in unsmoothed){
        ret[[i]] <- object[[i]]
    }

    ## class and return
    control$nsessions <- length(session)
    control$what <- list(what)
    operations <- list()
    operations$smooth <- control
    attr(ret, "operations") <- operations
    attr(ret, "units") <- units
    class(ret) <- "distrProfile"
    return(ret)
}


#' Auxiliary function for \code{\link{smoother.distrProfile}}. Typically used to construct
#' a control argument for \code{\link{smoother.distrProfile}}.
#'
#' @param what Vector of the names of the variables which should be smoothed.
#' @inheritParams decreasingSmoother
#' @param parallel Logical. Should computation be carried out in parallel?
#' @param cores Number of cores for parallel computing. If NULL, the number of cores is set to the value of \code{options("corese")} (on Windows) or \code{options("mc.cores")} (elsewhere), or, if the relevant option is unspecified, to half the number of cores detected.
#' @export
smootherControl.distrProfile <- function(what = c("speed", "heart.rate"), k = 30, sp = NULL,
                                         ## len = NULL, fam = "poisson",
                                         parallel = FALSE, cores = NULL){
    if (is.vector(what)) {
        what <- list(what)
    }
    if (is.null(cores)){
        dc <- parallel::detectCores()
        if (.Platform$OS.type != "windows"){
            cores <- getOption("mc.cores", max(floor(dc/2), 1L))
        } else {
            cores <- getOption("cores", max(floor(dc/2), 1L))
        }
    }

    list(what = what, k = k, sp = sp, parallel = parallel, cores = cores)
}



#' Smooth a decreasing function.
#'
#' This smoother ensures a positive response (Poisson) that is a monotone decreasing function of x.
#' @param x The regressor passed on to the \code{formula} argument of \code{\link[scam]{scam}}.
#' @param y The response passed on to the \code{formula} argument of \code{\link[scam]{scam}}.
#' @param k Number of knots.
#' @param len If \code{NULL}, the default, \code{x} is used for prediction. Otherwise,
#'     prediction is done over the range of \code{x} with \code{len} equidistant points.
#' @param sp A vector of smoothing parameters passed on to \code{\link[scam]{scam}}.
#' @param fam A family object passed on to the \code{family} argument of \code{\link[scam]{scam}}.
decreasingSmoother <- function(x, y, k = 30, len = NULL, sp = NULL,
                               fam = "poisson") {
    dat <- data.frame(y = y, x = x)
    scamFormula <- stats::as.formula(paste0("y ~ s(x, k = ", k, ", bs = 'mpd')"))
    gamfit <- scam::scam(scamFormula, family = fam, sp = sp, data = dat)
    xmin <- min(x)
    xman <- max(x)
    if (is.null(len)) {
        predictionRange <- x
    }
    else {
        predictionRange <- seq(xmin, xman, length.out = len)
    }
    res <- list(x = predictionRange,
                y = stats::predict(gamfit, type = "response",
                    newdata = data.frame(x = predictionRange)))
    #class(res) <- "decreasingSmoother"
    res
}


#' Append training profiles.
#'
#' Generic function for appending training distribution or
#' concentration profiles to existing files.
#'
#' @param object The object to be appended.
#' @param file The file to which \code{object} is to be appended.
#' @param ... Currently not used.
#' @rdname append.xprofile
#' @export
append.distrProfile <- function(object, file, ...){
    old <- load(file)
    new <- c(old, object)
    save(new, file)
}


## FIXME: what about appending more "types of profiles" (aka variables)? different function?
#' @export
c.distrProfile <- function(..., recursive = FALSE){

    input <- list(...)
    ninput <- length(input)
    if (ninput < 2) return(input[[1]])

    ## all input objects need to contain profiles for the variables in the first input object
    ## missing profiles are not filled up with NA (yet? FIXME?)
    ## additional profiles are discarded.
    allNames <- lapply(input, names)
    if (!all(sapply(allNames, function(x) all(allNames[[1]] %in% x))))
        stop(paste0("All objects need to contain distribution profiles for the variables contained in the first object: ",
                   paste(allNames[[1]], collapse = ", "), "."))

    nsessionsInput <- sapply(input, length)
    operations <- getOperations(input[[1]])

    ## check/change smoother attribute

    ## if all smoother settings are NULL, skip whole aggregation process
    if (!all(sapply(input, function(x) is.null(getOperations(x)$smooth)))) {

        ## if the settings for the first session are NULL, create a new reference setup
        if (is.null(getOperations(input[[1]])$smooth)){
            operations$smooth <- list(what = NA, k = NA, sp = NA,
                                      parallel = FALSE, cores = NULL,
                                      nsessions = NULL)
        }

        whats <- lapply(input, function(x) unique(getOperations(x)$smooth$what))
        ks <- lapply(input, function(x) unique(getOperations(x)$smooth$k))
        sps <- lapply(input, function(x) unique(getOperations(x)$smooth$sp))
        changeWhat <- any(!sapply(whats, function(x) isTRUE(all.equal(whats[[1]], x))))
        changeK <- any(!sapply(ks, function(x) isTRUE(all.equal(ks[[1]], x))))
        changeSp <- any(!sapply(sps, function(x) isTRUE(all.equal(sps[[1]], x))))
        changeO <- changeWhat | changeK | changeSp
        if (changeO) {
            whats <- lapply(input, function(x) getOperations(x)$smooth$what)
            whats[sapply(whats, is.null)] <- list(operations$smooth$what[1])
            whats <- do.call("c", whats)

            ks <- lapply(input, function(x) getOperations(x)$smooth$k)
            ks[sapply(ks, is.null)] <- operations$smooth$k[1]
            ks <- do.call("c", ks)

            sps <- lapply(input, function(x) getOperations(x)$smooth$sp)
            sps[sapply(sps, is.null)] <- list(operations$smooth$sp[1])
            sps <- do.call("c", sps)

            nsessions <- lapply(input, function(x) getOperations(x)$smooth$nsessions)
            nsessions[sapply(nsessions, is.null)] <- nsessionsInput[sapply(nsessions, is.null)]
            nsessions <- do.call("c", nsessions)

            operations$smooth$what <- whats
            operations$smooth$k <- ks
            operations$smooth$sp <- sps
            operations$smooth$nsessions <- nsessions
        } else {
            nsessions <- lapply(input, function(x) getOperations(x)$smooth$nsessions)
            nsessions[sapply(nsessions, is.null)] <- nsessionsInput[sapply(nsessions, is.null)]
            operations$smooth$nsessions <- sum(do.call("c", nsessions))
        }
    }

    units1 <- getUnits(input[[1]])
    units <- lapply(input, attr, "units")
    changeU <- !all(sapply(units, function(x) isTRUE(all.equal(units1, x))))
    if(changeU) {
        warning("The profiles for at least one variable have different units. The units of the first profile for each variable are applied to all profiles of that variable.")
        ## change units
        for (i in 2:ninput){
            input[[i]] <- changeUnits(input[[i]], variable = units1$variable, unit = units1$unit)

        }
    }

    ret <- list()
    what <- names(input[[1]])
    for (i in what){
        input_i <- lapply(input, function(x) x[[i]])
        ## FIXME: needs some tolerance for cases where the underlying grid is the same apart from minor numerical differences.
        ret[[i]] <- do.call("merge", input_i)
        attr(ret[[i]], "dimnames") <- list(NULL, paste0("Session", 1:ncol(ret[[i]])))
    }

    class(ret) <- c("distrProfile", class(ret))
    attr(ret, "operations") <- operations
    attr(ret, "units") <- units1
    return(ret)
}


## FIXME: implement [] method

