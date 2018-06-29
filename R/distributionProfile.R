#' Generate training distribution profiles.
#'
#' @param object An object of class \code{\link{trackeRdata}}.
#' @param session A numeric vector of the sessions to be used,
#'     defaults to all sessions.
#' @param what The variables for which the distribution profiles
#'     should be generated.
#' @param grid A named list containing the grid values for the
#'     variables in \code{what}. If \code{NULL} (default) the grids
#'     for the variables in \code{what} are inferred from
#'     \code{object}.
#' @param parallel Logical. Should computation be carried out in
#'     parallel? Default is \code{FALSE}.
#' @return An object of class \code{distrProfile}.
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
#' dProfile <- distributionProfile(run, what = c("speed", "cadence"), grid = seq(0, 12.5, by = 0.05))
#' plot(dProfile, smooth = FALSE)
#' @export
distribution_profile <- function(object,
                                 session = NULL,
                                 what = c("speed", "heart_rate"),
                                 grid = NULL,
                                 parallel = FALSE,
                                 unit_reference_sport = NULL) {

    if (is.null(session)) {
        session <- 1:length(object)
    }
    object <- object[session]

    if (is.null(grid)) {
        ## Fortify can be extremely slow for large objects...
        limits <- compute_limits(object, a = 0.01)
        for (feature in what) {
            if (all(is.na(limits[feature, ]))) {
                warning(paste('No data for', feature))
                what <- what[!(what %in% feature)]
            }
        }
        for (feature in what) {
            grid[[feature]] <- clean_grid(limits[feature, "low"], limits[feature, "upp"])
        }
    }

    units <- get_units(object)
    if (is.null(unit_reference_sport)) {
        unit_reference_sport <- find_unit_reference_sport(object)
    }
    ## Match units to those of unit_reference_sport
    un <- collect_units(units, unit_reference_sport)
    for (va in unique(un$variable)) {
        units$unit[units$variable == va] <- un$unit[un$variable == va]
    }

    ## check supplied args
    ## if it's a list, it has to either has to be named and contain all element in what or
    ## has to have the same length as what, then it's assumed that the order is the same.
    if (is.list(grid)) {
        if (is.null(names(grid)) & length(what) == length(grid)) {
            names(grid) <- what
        }
        if (is.null(names(grid))) {
            stop("Can't match variables in argument 'what' and their grid. Please provide a named list.")
        }
        if (any(is.na(match(what, names(grid))))) {
            stop("Please provide a grid for all variables in argument 'what'.")
        }
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

    stopifnot(!any(is.na(match(what, names(grid)))))

    duration_unit <- un$unit[un$variable == "duration"]
    du <- switch(duration_unit, "s" = "secs", "min" = "mins", "h" = "hours", "d" = "days")
    dp <- function(sess, grid) {
        values <- coredata(sess)
        timestamps <- index(sess)
        len <- difftime(max(timestamps), min(timestamps), unit = du)
        ## Remove NA
        missing <- is.na(values)
        values <- values[!missing]
        timestamps <- timestamps[!missing]
        ## Return NA if all values are missing
        if (all(missing)) return(rep(NA, length(grid)))
        ## Calculate dp
        charOrder <- order(values)
        values <- values[charOrder]
        uniqueValues <- unique(values)
        ## This specification of weights matches the result of time above threshold with ge = FALSE
        weights <- c(difftime(timestamps[-1], timestamps[-length(timestamps)], units = du), 0)[charOrder]
        weights <- sapply(uniqueValues, function(xx) sum(weights[values == xx]))
        out <- stats::approx(x = uniqueValues, y = cumsum(weights)/sum(weights), xout = grid,
                      method = "constant", yleft = 0, yright = 1, f = 0, ties = "ordered")$y
        ## Can be included for scaling
        ## if (scaled)
        ##     1 - out
        ## else
        ## Multiply by len instead of sum(weights) to ensure matching the session length
        len * (1 - out)
    }

    dp_fun <- function(j, w) {
        sess <- object[[j]]
        dp(sess[, w], grid[[w]])
    }

    DP <- list()
    for (i in what) {
        foreach_object <- eval(as.call(c(list(quote(foreach::foreach),
                                              j = seq.int(nsessions(object)),
                                              .combine = "cbind"))))
        if (parallel) {
            dp_for_var <- foreach::`%dopar%`(foreach_object, dp_fun(j, i))
        }
        else {
            dp_for_var <- foreach::`%do%`(foreach_object, dp_fun(j, i))
        }
        DP[[i]] <- zoo(dp_for_var, order.by = grid[[i]])
        names(DP[[i]]) <- paste0("Session", session)
    }

    operations <- list(smooth = NULL)
    attr(DP, "unit_reference_sport") <- unit_reference_sport
    attr(DP, "operations") <- operations
    attr(DP, "units") <- units
    class(DP) <- "distrProfile"
    return(DP)
}

#' Scale the distribution profile relative to its maximum value
#'
#' @param object An object of class \code{distrProfile} as returned by
#'     \code{\link{distributionProfile}}.
#' @param session A numeric vector of the sessions to be plotted,
#'     defaults to all sessions.
#' @param what Which variables should be scaled?
#' @param ... Currently not used.
#' @export
scaled.distrProfile <- function(object, session = NULL, what = c("speed", "heart_rate"), ...){
    ## select sessions
    availSessions <- if (is.null(ncol(object[[1]]))) 1 else ncol(object[[1]])
    if (is.null(session)) session <- 1:availSessions
    for (i in seq_along(object)) object[[i]] <- object[[i]][,session, drop = FALSE]

    ret <- list()
    what <- unlist(what)[unlist(what) %in% names(object)]

    for (i in what){
        nc <- if (is.null(ncol(object[[1]]))) 1 else ncol(object[[1]])
        cdat <- coredata(object[[i]])
        scaledProfile <- sweep(cdat, 2, apply(cdat, 2, max), "/")
        colnames(scaledProfile) <- attr(object[[i]], "dimnames")[[2]]
        ret[[i]] <- zoo(scaledProfile, order.by = index(object[[i]]))
    }

    unscaled <- names(object)[!(names(object) %in% what)]
    for (i in unscaled){
        ret[[i]] <- object[[i]]
    }

    ## class and return
    operations <- get_operations(object)
    attr(ret, "operations") <- c(operations, list(scale = TRUE))
    attr(ret, "units") <- getUnits(object)
    class(ret) <- "distrProfile"
    return(ret)
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
#' @param x An object of class \code{distrProfile} as returned by
#'     \code{\link{distributionProfile}}.
#' @param session A numeric vector of the sessions to be plotted,
#'     defaults to all sessions.
#' @param what Which variables should be plotted?
#' @param multiple Logical. Should all sessions be plotted in one
#'     panel?
#' @param smooth Logical. Should unsmoothed profiles be smoothed
#'     before plotting?
#' @param ... Further arguments to be passed to
#'     \code{\link{smoother_control.distrProfile}}.
#' @examples
#' data('runs', package = 'trackeR')
#' dProfile <- distributionProfile(runs, session = 1:2,
#'     what = "speed", grid = seq(0, 12.5, by = 0.05))
#' plot(dProfile, smooth = FALSE)
#' plot(dProfile, smooth = FALSE, multiple = TRUE)
#' plot(dProfile, multiple = TRUE)
#' @export
plot.distrProfile <- function(x, session = NULL,
                              what = c("speed", "heart_rate"),
                              multiple = FALSE,
                              smooth = FALSE, ...) {
    ## code inspired by autoplot.zoo
    units <- getUnits(x)
    ## Match units to those of unit_reference_sport
    un <- collect_units(units, attr(x, "unit_reference_sport"))
    for (va in unique(un$variable)) {
        units$unit[units$variable == va] <- un$unit[un$variable == va]
    }
    duration_unit <- un$unit[un$variable == "duration"]

    operations <- get_operations(x)

    ## select variables
    what <- what[what %in% names(x)]
    x <- x[what] ## FIXME: implement [] method for profiles/variables instead of sessions
    class(x) <- "distrProfile"
    attr(x, "operations") <- operations
    attr(x, "unit") <- units

    ## select sessions
    availSessions <- if (is.null(ncol(x[[1]]))) 1 else ncol(x[[1]])
    if (is.null(session)) {
        session <- 1:availSessions
    }
    for (i in what) {
        x[[i]] <- x[[i]][,session]
    }

    ## smooth
    if (smooth) {
        if (!is.null(operations$smooth)) {
            warning("This object has already been smoothed. No additional smoothing takes place.")
        }
        else {
            x <- smoother(x, what = what, ...)
        }
    }

    ## get data
    rownames(x) <- NULL
    df <- fortify(x, melt = TRUE)
    if (length(session) < 2) {
        df$Series <- session
    }
    else {
        df$Series <- as.numeric(sapply(strsplit(as.character(df$Series), "Session"), function(x) x[2]))
    }
    df$Profile <- factor(df$Profile)

    ## make basic plot and facets
    lab_data <- function(series) {
        thisunit <- un$unit[un$variable == series]
        prettyUnit <- prettifyUnits(thisunit)
        paste0(series, " [", prettyUnit,"]")
    }
    lab_data <- Vectorize(lab_data)

    if (multiple) {
        p <- ggplot(data = df, aes_(x = quote(Index), y = quote(Value),
                                    group = quote(Series), color = quote(Series)))
        facets <- ". ~ Profile"
    }
    else {
        p <- ggplot(data = df, mapping = aes_(x = quote(Index), y = quote(Value)))
        facets <- "Series ~ Profile"
    }

    ## add facets if necessary
    if (!is.null(facets)){
        p <- p + geom_line(na.rm = TRUE) +
            facet_grid(facets, scales = "free_x", labeller = labeller("Profile" = lab_data)) +
            ylab(paste0("Time spent above threshold", " [", duration_unit, "]")) +
            xlab("")
    }

    ## add bw theme
    p <- p + theme_bw() + scale_colour_continuous(name = "Session")

    return(p)
}

#' Smoother for distribution profiles.
#'
#' The distribution profiles are smoothed using a shape constrained
#' additive model with Poisson responses to ensure that the smoothed
#' distribution profile is positive and monotone decreasing.
#'
#' @param object An object of class \code{distrProfile} as returned by
#'     \code{\link{distributionProfile}}.
#' @param session A numeric vector of the sessions to be selected and
#'     smoothed. Defaults to all sessions.
#' @param control A list of parameters for controlling the smoothing
#'     process.  This is passed to
#'     \code{\link{smoother_control.distrProfile}}.
#' @param ... Arguments to be used to form the default \code{control}
#'     argument if it is not supplied directly.
#' @seealso \code{\link{smoother_control.distrProfile}}
#' @references
#'
#' Kosmidis, I., and Passfield, L. (2015). Linking the Performance of
#' Endurance Runners to Training and Physiological Effects via
#' Multi-Resolution Elastic Net. \emph{ArXiv e-print}
#' arXiv:1506.01388.
#'
#' Pya, N. and Wood S. (2015). Shape Constrained Additive
#' Models. Statistics and Computing, 25(3), 543--559.  Frick, H.,
#' Kosmidis, I. (2017). trackeR: Infrastructure for Running and
#' Cycling Data from GPS-Enabled Tracking Devices in R. \emph{Journal
#' of Statistical Software}, \bold{82}(7),
#' 1--29. doi:10.18637/jss.v082.i07
#'
#' @export
smoother.distrProfile <- function(object,
                                  session = NULL,
                                  control = list(...),
                                  ...) {
    units <- getUnits(object)
    ## evaluate control argument
    control <- do.call("smoother_control.distrProfile", control)
    ## select sessions
    availSessions <- if (is.null(ncol(object[[1]]))) 1 else ncol(object[[1]])
    if (is.null(session)) {
        session <- 1:availSessions
    }
    for (i in seq_along(object)) {
        object[[i]] <- object[[i]][, session]
    }

    ## smooth
    ret <- list()
    what <- unlist(control$what)[unlist(control$what) %in% names(object)]

    smooth_fun <- function(j, w) {
        sess <- object[[w]][, j]
        decreasing_smoother(x = index(sess),
                           y = coredata(sess),
                           k = control$k, len = NULL, sp = control$sp)$y
    }

    for (i in what) {
        nc <- if (is.null(ncol(object[[1]]))) 1 else ncol(object[[1]])
        foreach_object <- eval(as.call(c(list(quote(foreach::foreach),
                                              j = seq.int(nc),
                                              .combine = "cbind"))))
        ## move cbind in order to get the right dim always
        if (control$parallel) {
            sp <- foreach::`%dopar%`(foreach_object, cbind(smooth_fun(j, i)))
        }
        else {
            sp <- foreach::`%do%`(foreach_object, cbind(smooth_fun(j, i)))
        }

        colnames(sp) <- attr(object[[i]], "dimnames")[[2]]
        ret[[i]] <- zoo(sp, order.by = index(object[[i]]))
    }

    unsmoothed <- names(object)[!(names(object) %in% what)]
    for (i in unsmoothed) {
        ret[[i]] <- object[[i]]
    }

    ## class and return
    control$nsessions <- length(session)
    control$what <- list(what)
    operations <- list()
    operations$smooth <- control
    attr(ret, "unit_reference_sport") <- attr(object, "unit_reference_sport")
    attr(ret, "operations") <- operations
    attr(ret, "units") <- units
    class(ret) <- "distrProfile"
    return(ret)
}


#' Auxiliary function for
#' \code{\link{smoother.distrProfile}}. Typically used to construct a
#' control argument for \code{\link{smoother.distrProfile}}.
#'
#' @param what Vector of the names of the variables which should be
#'     smoothed.
#' @inheritParams decreasing_smoother
#' @param parallel Logical. Should computation be carried out in
#'     parallel?
#' @export
smoother_control.distrProfile <- function(what = c("speed", "heart_rate"),
                                         k = 30,
                                         sp = NULL,
                                         parallel = FALSE) {
    if (is.vector(what)) {
        what <- list(what)
    }
    list(what = what, k = k, sp = sp, parallel = parallel, cores = cores)
}

#' Smooth a decreasing function
#'
#' This smoother ensures a positive response that is a monotone decreasing function of x.
#' @param x The regressor passed on to the \code{formula} argument of \code{\link[scam]{scam}}.
#' @param y The response passed on to the \code{formula} argument of \code{\link[scam]{scam}}.
#' @param k Number of knots.
#' @param len If \code{NULL}, the default, \code{x} is used for prediction. Otherwise,
#'     prediction is done over the range of \code{x} with \code{len} equidistant points.
#' @param sp A vector of smoothing parameters passed on to \code{\link[scam]{scam}}.
#'
#' @export
decreasing_smoother <- function(x, y, k = 30, len = NULL, sp = NULL) {
    if (all(is.na(y))) {
        return(list(x = x, y = rep(NA, length(x))))
    }
    a = 0.0001
    ## bring y in [0, 1]
    my <- max(y)
    y <- y/ifelse(my == 0, 1, my)
    ## transform to real
    y <- log((y + a)/(1 - y + a))
    dat <- data.frame(x, y)
    scamFormula <- stats::as.formula(paste0("y ~ s(x, k = ", k, ", bs = 'mpd')"))
    gamfit <- scam::scam(scamFormula, family = gaussian(link = "identity"), data = dat)
    y <- stats::predict(gamfit, type = "response", newdata = data.frame(x = x))
    ## Transform back
    y <- (plogis(y) * (1 + 2*a) - a) * my
    res <- list(x = x,
                y = y)
    res
}

#' @export
nsessions.distrProfile <- function(object, ...) {
    if (is.null(ncol(object[[1]]))) 1 else ncol(object[[1]])
}


#' Ridgeline plots for \code{distrProfile} objects
#'
#' @inheritParams plot.distrProfile
#'
#' @examples
#' \dontrun{
#'
#' data('runs', package = 'trackeR')
#' dProfile <- distributionProfile(runs, what = c("speed", "heart_rate"), auto_grid = TRUE)
#' ridges(dProfile)
#'
#' }
ridges.distrProfile <- function(x, session = NULL, what = c("speed"),
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

    if (length(session) < 2) {
        df$Series <- session
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
