#' Generate training concentration profiles.
#'
#' @param object An object of class \code{distrProfile} as returned by \code{\link{distributionProfile}}.
#' @param what The variables for which the concentration profiles should be generated.
#' @inheritParams distributionProfile
#' @param ... Currently not used.
#' @return An object of class \code{conProfile}.
#' @references Kosmidis, I., and Passfield, L. (2015). Linking the Performance of
#'     Endurance Runners to Training and Physiological Effects via Multi-Resolution
#'     Elastic Net. \emph{ArXiv e-print} arXiv:1506.01388.
#' @examples
#' data(run, package = "trackeR")
#' dProfile <- distributionProfile(run, what = "speed", grid = seq(0, 12.5, by = 0.05))
#' cProfile <- concentrationProfile(dProfile)
#' plot(cProfile, smooth = FALSE)
#' plot(cProfile)
#' @export
concentrationProfile <- function(object, session = NULL, what = c("speed", "heart.rate"), ...){
    units <- getUnits(object)
    operations <- getOperations(object)
    
    ## select variables
    what <- what[what %in% names(object)]
    object <- object[what] ## FIXME: implement [] method profiles/variables instead of sessions
    class(object) <- "conProfile"; attr(object, "operations") <- operations; attr(object, "unit") <- units

    ## select sessions
    availSessions <- if (is.null(ncol(object[[1]]))) 1 else ncol(object[[1]])
    if (is.null(session)) session <- 1:availSessions
    for(i in what) object[[i]] <- object[[i]][,session]

    ## get concentration profile
    CP <- list()
    for (i in what){
        CP[[i]] <- - diff(object[[i]]) / diff(index(object[[i]]))
    }

    ## class and return
    attr(CP, "operations") <- operations
    attr(CP, "units") <- units
    class(CP) <- "conProfile"
    return(CP)
}


#' Fortify a conProfile object for plotting with ggplot2.
#'
#' @param model The \code{conProfile} object.
#' @inheritParams fortify.distrProfile
#' @export
fortify.conProfile <- function(model, data, melt = FALSE, ...){
    ret <- list()
    for (i in seq_along(model)){

        ret[[i]] <- zoo::fortify.zoo(model[[i]], melt = melt)
        ret[[i]]$Profile <- names(model)[i]
    }
    ret <- do.call("rbind", ret)
    return(ret)
}


## README: more examples, especially for the behaviour of session?
#' Plot concentration profiles.
#'
#' @param x An object of class \code{conProfile} as returned by \code{\link{concentrationProfile}}.
#' @param session A vector of the sessions to be plotted, defaults to all sessions.
#'     Either a character vector with the session names, e.g., c("Session3", "Session4")
#'     or a numeric vector with the relative position of the session(s).
#' @param what Which variables should be plotted?
#' @param multiple Logical. Should all sessions be plotted in one panel?
#' @param smooth Logical. Should unsmoothed profiles be smoothed before plotting?
#' @param ... Currently not used.
#' @examples
#' data(runs, package = "trackeR")
#' dProfile <- distributionProfile(runs, session = 1:3,
#'     what = "speed", grid = seq(0, 12.5, by = 0.05))
#' cProfile <- concentrationProfile(dProfile)
#' plot(cProfile, smooth = FALSE)
#' plot(cProfile)
#' @export
plot.conProfile <- function(x, session = NULL, what = c("speed", "heart.rate"),
                            multiple = FALSE, smooth = TRUE, ...){
    ## code inspired by autoplot.zoo
    units <- getUnits(x)
    operations <- getOperations(x)
    
    ## select variables
    what <- what[what %in% names(x)]   
    x <- x[what] ## FIXME: implement [] method for profiles/variables instead of sessions
    class(x) <- "conProfile"; attr(x, "operations") <- operations; attr(x, "unit") <- units

    ## select sessions
    ## if (is.null(session)) {
    ##     session <- attr(x[[1]], "dimnames")[[2]] #1:ncol(x[[1]])
    ## } else {
    ##     if(is.numeric(session)) session <- attr(x[[1]], "dimnames")[[2]][session]
    ## }
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
    ## if (length(session) > 1L) df <- subset(df, Series %in% session)
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
        p <- ggplot2::ggplot(data = df, mapping = ggplot2::aes_(x = quote(Index),
                        y = quote(Value), group = quote(Series), color = quote(Series))) +
            ggplot2::geom_line() + ggplot2::ylab("dtime") +
                ggplot2::xlab(if(singleVariable) lab_data(levels(df$Profile)) else "")
        facets <- if(singleVariable) NULL else ". ~ Profile"
    } else {
        p <- ggplot2::ggplot(data = df, mapping = ggplot2::aes_(x = quote(Index), y = quote(Value))) +
            ggplot2::geom_line() + ggplot2::ylab("dtime") +
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


#' Transform concentration profile to distribution profile.
#'
#' @param cp Single concentration profile as a zoo object.
c2d <- function(cp){
    ct <- cp * c(diff(index(cp)),0)
    ret <- cumsum(coredata(ct))
    dp <- -(ret - ret[length(ret)])
    dp <- zoo(dp, order.by = index(cp))
    return(dp)
}


#' Smoother for concentration profiles.
#'
#' To ensure positivity of the smoothed concentration profiles, the concentration profiles
#' are transformed to distribution profiles before smoothing. The smoothed distribution
#' profiles are then transformed to concentration profiles.
#'
#' @param object An object of class \code{conProfile} as returned by \code{\link{concentrationProfile}}.
#' @param session A numeric vector of the sessions to be selected and smoothed. Defaults to all sessions.
#' @param control A list of parameters for controlling the smoothing process.
#'     This is passed to \code{\link{smootherControl.distrProfile}}. 
#' @param ... Arguments to be used to form the default \code{control} argument if it is not supplied directly.
#' @seealso \code{\link{smootherControl.distrProfile}}
#' @export
smoother.conProfile <- function(object, session = NULL, control = list(...), ...){
    units <- getUnits(object)
    
    ## transform to distribution profile
    DP <- list()
    for(i in names(object)){
        if (is.null(ncol(object[[i]]))) {
            DP[[i]] <- c2d(object[[i]])
        } else {
            dp <- matrix(NA, nrow = nrow(object[[i]]), ncol = ncol(object[[i]]))
            colnames(dp) <- attr(object[[i]], "dimnames")[[2]]
            for (j in seq_len(ncol(dp))){
                dpj <- c2d(object[[i]][,j])
                dp[,j] <- dpj
            }
            DP[[i]] <- zoo(dp, order.by = index(dpj))
        }
    }
    class(DP) <- "distrProfile"
    attr(DP, "operations") <- list(smooth = NULL)
    attr(DP, "units") <- units

    ## evaluate control argument
    control <- do.call("smootherControl.distrProfile", control)
   
    ## smooth distribution profile
    smoothDP <- smoother(DP, session = session, control)

    ## get concentration profile
    smoothCP <- concentrationProfile(smoothDP, what = unlist(control$what))

    return(smoothCP)
}


#' @inheritParams append
#' @inheritParams append.distrProfile
#' @rdname append.xprofile
#' @export
append.conProfile <- function(object, file, ...){
    old <- load(file)
    new <- c(old, object)
    save(new, file)
}


## FIXME: what about appending more "types of profiles" (aka variables)? different function?
#' @export
c.conProfile <- function(..., recursive = FALSE){
    
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

    class(ret) <- c("conProfile", class(ret))
    attr(ret, "operations") <- operations
    attr(ret, "units") <- units1
    return(ret) 
}

## FIXME: implement [] method
