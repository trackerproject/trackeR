#' Generate training distribution profiles
#'
#' @aliases distrProfile
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
#' @return
#'
#' An object of class \code{distrProfile}.
#'
#' Object:
#'
#' A named list with one or more components, corresponding to the
#' value of \code{what}. Each component is a matrix of dimension
#' \code{g} times \code{n}, where \code{g} is the length of the grids
#' set in \code{grid} (or 200 if \code{grid = NULL}) and \code{n} is
#' the number of sessions requested in the \code{session} argument.
#'
#' Attributes:
#'
#' Each \code{distrProfile} object carries the following attributes
#'
#' \itemize{
#'
#' \item \code{sport}: the sports corresponding to the columns of each
#' list component
#'
#' \item \code{session_times}: the session start and end times
#' correspoding to the columns of each list component
#'
#' \item \code{unit_reference_sport}: the sport where the units have
#' been inherited from
#'
#' \item \code{operations}: a list with the operations that have been
#' applied to the object. See \code{\link{get_operations.distrProfile}}
#'
#' \item \code{units}: an object listing the units used for the
#' calculation of distribution profiles. These is the output of
#' \code{\link{get_units}} on the corresponding
#' \code{\link{trackeRdata}} object, after inheriting units from
#' \code{unit_reference_sport}.
#'
#' }
#'
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
    times <- session_times(object)
    if (is.null(session)) {
        session <- 1:length(object)
    }
    object <- object[session]
    if (is.null(grid)) {
        ## Fortify can be extremely slow for large objects...
        limits <- compute_limits(object, a = 0.01)
        for (feature in what) {
            if (all(is.na(limits[feature, ]))) {
                warning(paste('no data for', feature))
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
            stop("can't match variables in argument 'what' and their grid. Please provide a named list.")
        }
        if (any(is.na(match(what, names(grid))))) {
            stop("please provide a grid for all variables in argument 'what'.")
        }
        grid <- grid[what]
    }
    else {
        if (length(what) == 1L & is.vector(grid)) {
            grid <- list(grid)
            names(grid) <- what
        }
        else {
            stop("arguments 'what' and 'grid' don't match.")
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
        names(DP[[i]]) <- paste0("session", session)
    }
    operations <- list(smooth = NULL)
    attr(DP, "sport") <- get_sport(object)
    attr(DP, "session_times") <- times[session, ]
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
#' @param session A numeric vector of the sessions to be selected and
#'     scaled. Defaults to all sessions.
#' @param what A character version of the variables to be selected and
#'     scaled. Defaults to \code{c('speed', 'heart_rate')}.
#' @param ... Currently not used.
#'
#' @export
scaled.distrProfile <- function(object,
                                session = NULL,
                                what = c("speed", "heart_rate"),
                                ...){
    object <- get_profile(object, session = session, what = what)
    what <- names(object)
    ## scale
    ret <- list()
    nc <- nsessions(object)
    for (i in what) {
        cdat <- coredata(object[[i]])
        scaledProfile <- sweep(cdat, 2, apply(cdat, 2, max), "/")
        colnames(scaledProfile) <- attr(object[[i]], "dimnames")[[2]]
        ret[[i]] <- zoo(scaledProfile, order.by = index(object[[i]]))
    }
    unsmoothed <- names(object)[!(names(object) %in% what)]
    for (i in unsmoothed) {
        ret[[i]] <- object[[i]]
    }

    ## class and return
    operations <- get_operations(object)
    attr(ret, "sport") <- get_sport(object)
    attr(ret, "session_times") <- attr(object, "session_times")
    attr(ret, "unit_reference_sport") <- attr(object, "unit_reference_sport")
    attr(ret, "operations") <- c(operations, list(scale = TRUE))
    attr(ret, "units") <- get_units(object)
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
fortify.distrProfile <- function(model,
                                 data,
                                 melt = FALSE,
                                 ...) {
    ret <- list()
    sport <- get_sport(model)
    times <- attr(model, "session_times")
    for (i in seq_along(model)){
        ret[[i]] <- zoo::fortify.zoo(model[[i]], melt = melt, stringsAsFactors = FALSE)
        ret[[i]]$Profile <- names(model)[i]
        ret[[i]]$Sport <- rep(sport, each = nrow(model[[i]]))
        ret[[i]]$Date <- rep(times[, 1], each = nrow(model[[i]]))
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
    x <- get_profile(x, session = session, what = what)
    ## smooth
    if (smooth) {
        if (!is.null(operations$smooth)){
            warning("this object has already been smoothed. No additional smoothing takes place.")
        }
        else {
            x <- smoother(x, what = what, ...)
        }
    }
    ## duration unit; sport does not matter here as units have been uniformised already
    units <- get_units(x)
    duration_unit <- units$unit[units$sport == "running" & units$variable == "duration"]
    ## fortify
    df <- fortify(x, melt = TRUE)
    df$Series <- as.numeric(sapply(strsplit(as.character(df$Series), "session"), function(x) x[2]))
    df$Profile <- factor(df$Profile)
    ## make basic plot and facets
    lab_data <- function(series) {
        thisunit <- units$unit[units$sport == "running" & units$variable == series]
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
    p + geom_line(na.rm = TRUE) +
        facet_grid(facets, scales = "free_x", labeller = labeller("Profile" = lab_data)) +
        ylab(paste0("Time spent above threshold", " [", duration_unit, "]")) +
        xlab("") +
        theme_bw() +
        scale_colour_continuous(name = "session")
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
#' @param what A character version of the variables to be selected and
#'     smoothed. Defaults to \code{c('speed', 'heart_rate')}.
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
#'
#' Kosmidis, I. (2017). trackeR: Infrastructure for Running and
#' Cycling Data from GPS-Enabled Tracking Devices in R. \emph{Journal
#' of Statistical Software}, \bold{82}(7),
#' 1--29. doi:10.18637/jss.v082.i07
#'
#' @export
smoother.distrProfile <- function(object,
                                  session = NULL,
                                  what = c("speed", "heart_rate"),
                                  control = list(...),
                                  ...) {
    ## evaluate control argument
    control <- do.call("smoother_control.distrProfile", control)
    object <- get_profile(object, session = session, what = what)
    what <- names(object)
    ## smooth
    smooth_fun <- function(j, w) {
        sess <- object[[w]][, j]
        decreasing_smoother(x = index(sess),
                           y = coredata(sess),
                           k = control$k, len = NULL, sp = control$sp)$y
    }
    ret <- list()
    nc <- nsessions(object)
    for (i in what) {
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
    operations <- list()
    operations$smooth <- control
    attr(ret, "sport") <- get_sport(object)
    attr(ret, "session_times") <- attr(object, "session_times")
    attr(ret, "unit_reference_sport") <- attr(object, "unit_reference_sport")
    attr(ret, "operations") <- operations
    attr(ret, "units") <- get_units(object)
    class(ret) <- "distrProfile"
    return(ret)
}


#' Auxiliary function for
#' \code{\link{smoother.distrProfile}}. Typically used to construct a
#' control argument for \code{\link{smoother.distrProfile}}.
#'
#' @inheritParams decreasing_smoother
#' @param parallel Logical. Should computation be carried out in
#'     parallel?
#' @export
smoother_control.distrProfile <- function(k = 30,
                                          sp = NULL,
                                          parallel = FALSE) {
    list(k = k, sp = sp, parallel = parallel)
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
    res <- list(x = x, y = y)
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
#' @export
ridges.distrProfile <- function(x,
                                session = NULL,
                                what = c("speed"),
                                smooth = FALSE,
                                ...){
    x <- get_profile(x, session = session, what = what)
    ## smooth
    if (smooth) {
        if (!is.null(operations$smooth)){
            warning("this object has already been smoothed. No additional smoothing takes place.")
        }
        else {
            x <- smoother(x, what = what, ...)
        }
    }
    ## duration unit; sport does not matter here as units have been uniformised already
    units <- get_units(x)
    duration_unit <- units$unit[units$sport == "running" & units$variable == "duration"]
    ## fortify
    df <- fortify(x, melt = TRUE)
    df$Series <- as.numeric(sapply(strsplit(as.character(df$Series), "session"), function(x) x[2]))
    df$Profile <- factor(df$Profile)
    ## make basic plot and facets
    lab_data <- function(series) {
        thisunit <- units$unit[units$sport == "running" & units$variable == series]
        prettyUnit <- prettifyUnits(thisunit)
        paste0(series, " [", prettyUnit,"]")
    }
    lab_data <- Vectorize(lab_data)
    sc <- 0.02
    ggplot(df) +
        ggridges::geom_ridgeline(aes_(x = quote(Index), y = quote(Series), height = quote(Value), group = quote(Series), scale = sc, fill = quote(Sport)), alpha = 0.5, color = gray(0.25, alpha = 0.1)) +
        ggridges::theme_ridges() +
        scale_fill_manual(values = c(cycling = "#76BD58", running = "#F68BA2", swimming = "#5EB3F0")) +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
        facet_grid(. ~ Profile, scales = "free_x", labeller = labeller("Profile" = lab_data)) +
        xlab("")
}


#' @rdname get_sport
#' @export
get_sport.distrProfile <- function(object,
                                   session = NULL,
                                   ...) {
    if (is.null(session)) {
        nc <- ncol(object[[1]])
        nc <- if (is.null(nc)) 1 else nc
        session <- seq.int(nc)
    }
    attr(object, "sport")[session]
}


#' Extract specific distribution and concentration profiles from
#' \code{\link{distrProfile}} and \code{\link{conProfile}} objects
#'
#' @param object An object of class \code{distrProfile} as returned by
#'     \code{\link{distributionProfile}}.
#' @param session A numeric vector of the sessions to selected.
#'     Defaults to all sessions.
#' @param what A character version of the variables to be
#'     selected. Defaults to \code{c('speed', 'heart_rate')}.
#' @export
get_profile <- function(object,
                        session = NULL,
                        what = c("speed", "heart_rate"),
                        ...) {
    sports <- get_sport(object)
    operations <- get_operations(object)
    units <- get_units(object)
    times <- attr(object, "session_times")
    urs <- attr(object, "unit_reference_sport")
    ## select variables
    inds <- what %in% names(object)
    if (all(!inds)) {
        stop("no data for ", paste0(what[!inds], collapse = ", "))
    }
    if (any(!inds)) {
        warning("no data for ", paste0(what[!inds], collapse = ", "))
    }
    what <- what[inds]
    nsess <- nsessions(object)
    ## Reset the object according to what and session
    object <- object[what]
    ## select sessions
    if (is.null(session)) {
        session <- seq.int(nsess)
    }
    for (i in what) {
        object[[i]] <- object[[i]][, session, drop = FALSE]
    }
    ## Re class
    attr(object, "sport") <- sports[session]
    attr(object, "session_times") <- times[session, ]
    attr(object, "unit_reference_sport") <- urs
    attr(object, "operations") <- operations
    attr(object, "units") <- units
    class(object) <- "distrProfile"
    object
}

