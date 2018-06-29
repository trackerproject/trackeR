#' Generate training concentration profiles.
#'
#' @param object An object of class \code{distrProfile} as returned by
#'     \code{\link{distributionProfile}}.
#' @param what The variables for which the concentration profiles
#'     should be generated.
#' @inheritParams distributionProfile
#' @param ... Currently not used.
#' @return An object of class \code{conProfile}.
#' @references
#'
#' Kosmidis, I., and Passfield, L. (2015). Linking the Performance of
#' Endurance Runners to Training and Physiological Effects via
#' Multi-Resolution Elastic Net. \emph{ArXiv e-print}
#' arXiv:1506.01388.
#'
#' Frick, H., Kosmidis, I. (2017). trackeR: Infrastructure for Running
#' and Cycling Data from GPS-Enabled Tracking Devices in
#' R. \emph{Journal of Statistical Software}, \bold{82}(7),
#' 1--29. doi:10.18637/jss.v082.i07
#'
#' @examples
#' data('run', package = 'trackeR')
#' dProfile <- distributionProfile(run, what = 'speed', grid = seq(0, 12.5, by = 0.05))
#' cProfile <- concentrationProfile(dProfile)
#' plot(cProfile, smooth = FALSE)
#' plot(cProfile)
#' @export
concentration_profile <- function(object,
                                  session = NULL,
                                  what = c("speed", "heart_rate"),
                                  ...) {
    units <- getUnits(object)
    operations <- get_operations(object)

    ## select variables
    what <- what[what %in% names(object)]
    object <- object[what]  ## FIXME: implement [] method profiles/variables instead of sessions
    attr(object, "operations") <- operations
    attr(object, "units") <- units
    class(object) <- "distrProfile"

    ## select sessions
    availSessions <- if (is.null(ncol(object[[1]])))
        1 else ncol(object[[1]])
    if (is.null(session))
        session <- 1:availSessions
    for (i in what) object[[i]] <- object[[i]][, session]

    ## get concentration profile
    CP <- list()
    for (i in what) {
        CP[[i]] <- -diff(object[[i]])/diff(index(object[[i]]))
    }

    ## class and return
    attr(CP, "operations") <- operations
    attr(CP, "units") <- units
    class(CP) <- "conProfile"
    return(CP)
}

## Experimental concentration profile
cp <- function(object,
               session = NULL,
               what = c("speed", "heart_rate"),
               limits = NULL,
               parallel = FALSE,
               unit_reference_sport = NULL) {

    units <- get_units(object)


    if (is.null(session)) {
        session <- 1:length(object)
    }
    object <- object[session]

    CP <- NULL

    ## If limits is NULL the limits are selected automatically using
    ## the same idea as auto_grid in dp
    if (is.null(limits)) {
        stop("limits = NULL will work in the future")
        df <- fortify(object, melt = FALSE)
        for (feature in what) {
            if (all(is.na(df[[feature]]))) {
                warning(paste('No data for', feature))
                what <- what[!(what %in% feature)]
            }
        }
        for (feature in what) {
            maximum <- ceiling(quantile(df[feature], 0.99999, na.rm = TRUE))
            minimum <- if (feature == 'heart_rate') 35 else 0
            limits[[feature]] <- c(minimum, maximum)
        }
    }


    cp_fun <- function(j, w) {
        sess <- object[[j]]
        values <- sess[, w]
        if (all(is.na(values))) {
            rep(NA, 512)
        }
        else {
            density(values, na.rm = TRUE, from = limits[[w]][1], to = limits[[w]][2], n = 512)$y
        }
    }

    for (i in what) {
        foreach_object <- eval(as.call(c(list(quote(foreach::foreach),
                                              j = seq.int(nsessions(object)),
                                              .combine = "cbind"))))
        if (parallel) {
            setup_parallel()
            times <- foreach::`%dopar%`(foreach_object, cp_fun(j, i))
        }
        else {
            times <- foreach::`%do%`(foreach_object, cp_fun(j, i))
        }

        times <- zoo(times, order.by = seq(from = limits[[i]][1], to = limits[[i]][2], length.out = 512))
        names(times) <- paste0("Session", session)
        CP[[i]] <- times
    }

    ## class and return
    operations <- list(smooth = TRUE)
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
fortify.conProfile <- function(model, data, melt = FALSE, ...) {
    ret <- list()
    for (i in seq_along(model)) {

        ret[[i]] <- zoo::fortify.zoo(model[[i]], melt = melt)
        ret[[i]]$Profile <- names(model)[i]
    }
    ret <- do.call("rbind", ret)
    return(ret)
}


## README: more examples, especially for the behaviour of session?
#' Plot concentration profiles.
#'
#' @param x An object of class \code{conProfile} as returned by
#'     \code{\link{concentrationProfile}}.
#' @param session A vector of the sessions to be plotted, defaults to
#'     all sessions.  Either a character vector with the session
#'     names, e.g., c('Session3', 'Session4') or a numeric vector with
#'     the relative position of the session(s).
#' @param what Which variables should be plotted?
#' @param multiple Logical. Should all sessions be plotted in one
#'     panel?
#' @param smooth Logical. Should unsmoothed profiles be smoothed
#'     before plotting?
#' @param ... Currently not used.
#' @examples
#' data('runs', package = 'trackeR')
#' dProfile <- distributionProfile(runs, session = 1:3, what = 'speed',
#'                                 grid = seq(0, 12.5, by = 0.05))
#' cProfile <- concentrationProfile(dProfile)
#' plot(cProfile, smooth = FALSE)
#' plot(cProfile)
#' @export
plot.conProfile <- function(x, session = NULL, what = c("speed", "heart_rate"), multiple = FALSE,
    smooth = TRUE, ...) {
    ## code inspired by autoplot.zoo
    units <- getUnits(x)
    operations <- get_operations(x)

    ## select variables
    what <- what[what %in% names(x)]
    x <- x[what]  ## FIXME: implement [] method for profiles/variables instead of sessions
    class(x) <- "conProfile"
    attr(x, "operations") <- operations
    attr(x, "unit") <- units

    ## select sessions if (is.null(session)) { session <- attr(x[[1]], 'dimnames')[[2]]
    ## #1:ncol(x[[1]]) } else { if(is.numeric(session)) session <- attr(x[[1]],
    ## 'dimnames')[[2]][session] }
    availSessions <- if (is.null(ncol(x[[1]])))
        1 else ncol(x[[1]])
    if (is.null(session))
        session <- 1:availSessions
    for (i in what) x[[i]] <- x[[i]][, session]

    ## smooth
    if (smooth) {
        if (!is.null(operations$smooth)) {
            warning("This object has already been smoothed. No additional smoothing takes place.")
        } else {
            x <- smoother(x, what = what, ...)
        }
    }

    ## get data
    rownames(x) <- NULL
    df <- fortify(x, melt = TRUE)
    ## if (length(session) > 1L) df <- subset(df, Series %in% session) df <- subset(df,
    ## Profile %in% what) HACK: If there is only one session (=series) to be plotted, give
    ## it a proper name for multiple = TRUE.
    if (length(session) < 2) {
        df$Series <- session  ## paste0('Session', session)
        ## df$Series <- factor(df$Series)
    } else {
        df$Series <- as.numeric(sapply(strsplit(as.character(df$Series), "Session"), function(x) x[2]))
    }
    df$Profile <- factor(df$Profile)

    ## ## check that there is data to plot for(l in levels(df$Series)){ if
    ## (all(is.na(subset(df, Series == l, select = 'Value')))) df <- df[!(df$Series == l), ]
    ## }

    ## make basic plot and facets
    singleVariable <- nlevels(df$Profile) == 1L
    singleSession <- nlevels(df$Series) == 1L
    lab_data <- function(series) {
        thisunit <- units$unit[units$variable == series]
        prettyUnit <- prettifyUnits(thisunit)
        paste0(series, " [", prettyUnit, "]")
    }
    lab_data <- Vectorize(lab_data)

    if (multiple) {
        p <- ggplot(data = df, mapping = aes_(x = quote(Index), y = quote(Value),
            group = quote(Series), color = quote(Series))) + geom_line(na.rm = TRUE) +
            ylab("dtime") + xlab(if (singleVariable)
            lab_data(levels(df$Profile)) else "")
        facets <- if (singleVariable)
            NULL else ". ~ Profile"
    } else {
        p <- ggplot(data = df, mapping = aes_(x = quote(Index), y = quote(Value))) +
            geom_line(na.rm = TRUE) + ylab("dtime") + xlab(if (singleVariable)
            lab_data(levels(df$Profile)) else "")

        facets <- if (singleVariable) {
            if (singleSession)
                NULL else "Series ~ ."
        } else {
            if (singleSession)
                ". ~ Profile" else "Series ~ Profile"
        }
    }

    ## add facets if necessary
    if (!is.null(facets)) {
        p <- p + facet_grid(facets, scales = "free_x", labeller = labeller(Profile = lab_data))
    }

    ## add bw theme
    p <- p + theme_bw() + scale_colour_continuous(name = "Session")

    return(p)
}


#' Transform concentration profile to distribution profile.
#'
#' @param cp Single concentration profile as a zoo object.
c2d <- function(cp) {
    ct <- cp * c(diff(index(cp)), 0)
    ret <- cumsum(coredata(ct))
    dp <- -(ret - ret[length(ret)])
    dp <- zoo(dp, order.by = index(cp))
    return(dp)
}


#' Smoother for concentration profiles.
#'
#' To ensure positivity of the smoothed concentration profiles, the
#' concentration profiles are transformed to distribution profiles
#' before smoothing. The smoothed distribution profiles are then
#' transformed to concentration profiles.
#'
#' @param object An object of class \code{conProfile} as returned by
#'     \code{\link{concentrationProfile}}.
#' @param session A numeric vector of the sessions to be selected and
#'     smoothed. Defaults to all sessions.
#' @param control A list of parameters for controlling the smoothing
#'     process.  This is passed to
#'     \code{\link{smootherControl.distrProfile}}.
#' @param ... Arguments to be used to form the default \code{control}
#'     argument if it is not supplied directly.
#' @seealso \code{\link{smootherControl.distrProfile}}
#' @export
smoother.conProfile <- function(object, session = NULL, control = list(...), ...) {
    units <- getUnits(object)

    ## transform to distribution profile
    DP <- list()
    for (i in names(object)) {
        if (is.null(ncol(object[[i]]))) {
            DP[[i]] <- c2d(object[[i]])
        } else {
            dp <- matrix(NA, nrow = nrow(object[[i]]), ncol = ncol(object[[i]]))
            colnames(dp) <- attr(object[[i]], "dimnames")[[2]]
            for (j in seq_len(ncol(dp))) {
                dpj <- c2d(object[[i]][, j])
                dp[, j] <- dpj
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

#' @export
nsessions.conProfile <- function(object, ...) {
    if (is.null(ncol(object[[1]])))
        1 else ncol(object[[1]])
}

#' Ridgeline plots for \code{distrProfile} objects
#'
#' @inheritParams plot.conProfile
#'
#' @examples
#' \dontrun{
#'
#' data('runs', package = 'trackeR')
#' dProfile <- distributionProfile(runs, what = c('speed', 'heart_rate'), auto_grid = TRUE)
#' cProfile <- concentrationProfile(dProfile)
#' ridges(cProfile, what = "speed")
#' ridges(cProfile, what = "heart_rate")
#' }
#'
ridges.conProfile <- function(x, session = NULL, what = c("speed"),
                              smooth = TRUE, ...){

    ## code inspired by autoplot.zoo
    units <- getUnits(x)
    operations <- get_operations(x)

    ## select variables
    what <- what[what %in% names(x)]
    if (length(what) > 1) {
        warnings(paste("Only", what[1], "is plotted"))
        what <- what[1]
    }
    x <- x[what]  ## FIXME: implement [] method for profiles/variables instead of sessions
    class(x) <- "conProfile"
    attr(x, "operations") <- operations
    attr(x, "unit") <- units

    ## select sessions if (is.null(session)) { session <- attr(x[[1]], 'dimnames')[[2]]
    ## #1:ncol(x[[1]]) } else { if(is.numeric(session)) session <- attr(x[[1]],
    ## 'dimnames')[[2]][session] }
    availSessions <- if (is.null(ncol(x[[1]])))
        1 else ncol(x[[1]])
    if (is.null(session))
        session <- 1:availSessions
    for (i in what) x[[i]] <- x[[i]][, session]

    ## smooth
    if (smooth) {
        if (!is.null(operations$smooth)) {
            warning("This object has already been smoothed. No additional smoothing takes place.")
        } else {
            x <- smoother(x, what = what, ...)
        }
    }

    ## get data
    rownames(x) <- NULL
    df <- fortify(x, melt = TRUE)
    ## if (length(session) > 1L) df <- subset(df, Series %in% session) df <- subset(df,
    ## Profile %in% what) HACK: If there is only one session (=series) to be plotted, give
    ## it a proper name for multiple = TRUE.
    if (length(session) < 2) {
        df$Series <- session  ## paste0('Session', session)
        ## df$Series <- factor(df$Series)
    } else {
        df$Series <- as.numeric(sapply(strsplit(as.character(df$Series), "Session"), function(x) x[2]))
    }
    df$Profile <- factor(df$Profile)


    singleSession <- nlevels(df$Series) == 1L
    lab_data <- function(series){
        thisunit <- units$unit[units$variable == series]
        prettyUnit <- prettifyUnits(thisunit)
        paste0(series, " [", prettyUnit,"]")
    }
    lab_data <- Vectorize(lab_data)

    sc <- 2/max(df$Value)
    ggplot(df) +
        ggridges::geom_ridgeline(aes_(x = quote(Index), y = quote(Series), height = quote(Value), group = quote(Series), scale = sc), alpha = 0.5) +
            ggridges::theme_ridges() +
            labs(x = lab_data(what), y = "Session")

}

