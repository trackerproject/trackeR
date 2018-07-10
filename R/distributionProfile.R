#' Generate training distribution profiles.
#'
#' @aliases distrProfile
#' @param object An object of class \code{\link{trackeRdata}}.
#' @param session A numeric vector of the sessions to be used,
#'     defaults to all sessions.
#' @param what The variables for which the distribution profiles
#'     should be generated. Defaults to all variables in
#'     \code{object} (\code{what = NULL}).
#' @param grid A named list containing the grid values for the
#'     variables in \code{what}. If \code{NULL} (default) the grids
#'     for the variables in \code{what} are inferred from
#'     \code{object}.
#' @param parallel Logical. Should computation be carried out in
#'     parallel? Default is \code{FALSE}.
#' @param unit_reference_sport The sport to inherit units from
#'     (default is taken to be the most frequent sport in
#'     \code{object}).
#' @return
#'
#' An object of class \code{distrProfile}.
#'
#' Object:
#'
#' A named list with one or more components, corresponding to the
#' value of \code{what}. Each component is a matrix of dimension
#' \code{g} times \code{n}, where \code{g} is the length of the grids
#' set in \code{grid} (or 201 if \code{grid = NULL}) and \code{n} is
#' the number of sessions requested in the \code{session} argument.
#'
#' Attributes:
#'
#' Each \code{distrProfile} object has the following attributes:
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
#' \item \code{limits}: The variable limits that have been used for the
#' computation of the distribution profiles
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
#' dProfile <- distribution_profile(run, what = c("speed", "cadence_running"))
#' plot(dProfile, smooth = FALSE)
#' @export
distribution_profile <- function(object,
                                 session = NULL,
                                 what = NULL,
                                 grid = NULL,
                                 parallel = FALSE,
                                 unit_reference_sport = NULL) {
    times <- session_times(object)
    if (is.null(session)) {
        session <- 1:length(object)
    }
    if (is.null(what)) {
        what <- colnames(object[[1]])
    }
    object <- object[session]
    units <- get_units(object)
    if (is.null(unit_reference_sport)) {
        unit_reference_sport <- find_unit_reference_sport(object)
    }
    ## Match units to those of unit_reference_sport
    un <- collect_units(units, unit_reference_sport)
    for (va in unique(un$variable)) {
        units$unit[units$variable == va] <- un$unit[un$variable == va]
    }
    ## Change units according to unit_reference_sport
    object <- change_units(object, units$variable, units$unit, units$sport)

    if (is.null(grid)) {
        ## Fortify can be extremely slow for large objects...
        limits <- compute_limits(object, a = 0.01)
        for (feature in what) {
            if (all(is.na(limits[[feature]]))) {
                warning(paste('no data for', feature))
                what <- what[!(what %in% feature)]
            }
        }
        for (feature in what) {
            grid[[feature]] <- clean_grid(limits[[feature]][1], limits[[feature]][2])
        }
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
        len <- difftime(max(timestamps), min(timestamps), units = du)
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
            dp_for_var <- foreach::`%dopar%`(foreach_object, cbind(dp_fun(j, i)))
        }
        else {
            dp_for_var <- foreach::`%do%`(foreach_object, cbind(dp_fun(j, i)))
        }

        if (nrow(dp_for_var) == 1) {
            warning("no data for ", i)
            next
        }
        DP[[i]] <- zoo(dp_for_var, order.by = grid[[i]])
        names(DP[[i]]) <- paste0("session", session)
    }
    if (length(DP) == 0) {
        stop("no usable data found")
    }
    operations <- list(smooth = NULL, scale = FALSE)
    attr(DP, "sport") <- get_sport(object)
    attr(DP, "session_times") <- times[session, ]
    attr(DP, "unit_reference_sport") <- unit_reference_sport
    attr(DP, "operations") <- operations
    attr(DP, "units") <- units
    attr(DP, "limits") <- if (is.null(grid)) limits[what] else lapply(grid, range)[what]
    class(DP) <- "distrProfile"
    return(DP)
}

#' Scale the distribution profile relative to its maximum value.
#'
#' @param object An object of class \code{distrProfile} as returned by
#'     \code{\link{distributionProfile}}.
#' @param session A numeric vector of the sessions to be selected and
#'     scaled. Defaults to all sessions.
#' @param what A character version of the variables to be selected and
#'     scaled. Defaults to all variables in \code{object} (\code{what
#'     = NULL}).
#' @param ... Currently not used.
#'
#' @export
scaled.distrProfile <- function(object,
                                session = NULL,
                                what = NULL,
                                ...) {
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
    ## class and return
    operations <- get_operations(object)
    operations$scale <- TRUE
    attr(ret, "sport") <- get_sport(object)
    attr(ret, "session_times") <- attr(object, "session_times")
    attr(ret, "unit_reference_sport") <- attr(object, "unit_reference_sport")
    attr(ret, "operations") <- operations
    attr(ret, "units") <- get_units(object)
    attr(ret, "limits") <- attr(object, "limits")[what]
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
    for (i in seq_along(model)) {
        ret[[i]] <- zoo::fortify.zoo(model[[i]], melt = melt, stringsAsFactors = FALSE)
        ret[[i]]$Profile <- names(model)[i]
        ret[[i]]$Sport <- rep(sport, each = nrow(model[[i]]))
        ret[[i]]$Date <- rep(times[, 1], each = nrow(model[[i]]))
    }
    ret <- do.call("rbind", ret)
    return(ret)
}


#' Fortify a \code{\link{conProfile}} object for plotting with ggplot2.
#'
#' @param model The \code{\link{conProfile}} object.
#' @inheritParams fortify.distrProfile
#' @export
fortify.conProfile <- fortify.distrProfile


#' Plot distribution profiles.
#'
#' @param x An object of class \code{distrProfile} as returned by
#'     \code{\link{distribution_profile}}.
#' @param session A numeric vector of the sessions to be plotted,
#'     defaults to all sessions.
#' @param what Which variables should be plotted? Defaults to all
#'     variables in \code{object} (\code{what = NULL}).
#' @param multiple Logical. Should all sessions be plotted in one
#'     panel?
#' @param smooth Logical. Should unsmoothed profiles be smoothed
#'     before plotting?
#' @param ... Further arguments to be passed to
#'     \code{\link{smoother_control.distrProfile}}.
#' @examples
#' data('runs', package = 'trackeR')
#' dProfile <- distribution_profile(runs, session = 1:2,
#'     what = "speed", grid = seq(0, 12.5, by = 0.05))
#' plot(dProfile, smooth = FALSE)
#' plot(dProfile, smooth = FALSE, multiple = TRUE)
#' plot(dProfile, multiple = TRUE)
#' @export
plot.distrProfile <- function(x, session = NULL,
                              what = NULL,
                              multiple = FALSE,
                              smooth = FALSE, ...) {
    x <- get_profile(x, session = session, what = what)
    ## smooth
    if (smooth) {
        x <- smoother(x, ...)
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
#'     \code{\link{distribution_profile}}.
#' @param session A numeric vector of the sessions to be selected and
#'     smoothed. Defaults to all sessions.
#' @param what A character version of the variables to be selected and
#'     smoothed. Defaults to all variables in \code{object}
#'     (\code{what = NULL}).
#' @param control A list of parameters for controlling the smoothing
#'     process.  This is passed to
#'     \code{\link{smoother_control.distrProfile}}.
#' @param ... Arguments to be used to form the default \code{control}
#'     argument if it is not supplied directly.
#' @seealso \code{\link{smoother_control.distrProfile}}
#'
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
                                  what = NULL,
                                  control = list(...),
                                  ...) {
    ## evaluate control argument
    control <- do.call("smoother_control.distrProfile", control)
    object <- get_profile(object, session = session, what = what)
    operations <- get_operations(object)
    if (!is.null(operations$smooth)) {
        warning("this object has already been smoothed. No additional smoothing takes place.")
        return(object)
    }
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
    ## class and return
    operations <- get_operations(object)
    operations$smooth <- control
    attr(ret, "sport") <- get_sport(object)
    attr(ret, "session_times") <- attr(object, "session_times")
    attr(ret, "unit_reference_sport") <- attr(object, "unit_reference_sport")
    attr(ret, "operations") <- operations
    attr(ret, "units") <- get_units(object)
    attr(ret, "limits") <- attr(object, "limits")[what]
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

#' Smooth a decreasing function.
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

#' @rdname nsessions
#' @export
nsessions.distrProfile <- function(object, ...) {
    if (is.null(ncol(object[[1]]))) 1 else ncol(object[[1]])
}

#' @rdname nsessions
#' @export
nsessions.conProfile <- nsessions.distrProfile


#' Ridgeline plots for \code{distrProfile} objects
#'
#' @inheritParams plot.distrProfile
#'
#' @examples
#' \dontrun{
#'
#' data('runs', package = 'trackeR')
#' dProfile <- distribution_profile(runs, what = c("speed", "heart_rate"))
#' ridges(dProfile)
#'
#' }
#' @export
ridges.distrProfile <- function(x,
                                session = NULL,
                                what = NULL,
                                smooth = FALSE,
                                ...){
    x <- get_profile(x, session = session, what = what)
    ## smooth
    if (smooth) {
        x <- smoother(x, ...)
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
        xlab("") + ylab("Session")
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


#' @rdname get_profile
#' @export
get_profile.distrProfile <- function(object,
                                      session = NULL,
                                      what = NULL,
                                      ...) {
    sports <- get_sport(object)
    operations <- get_operations(object)
    units <- get_units(object)
    times <- attr(object, "session_times")
    urs <- attr(object, "unit_reference_sport")
    limits <- attr(object, "limits")
    if (is.null(what)) {
        what <- names(object)
    }
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
    attr(object, "limits") <- limits[what]
    class(object) <- "distrProfile"
    object
}



#' @rdname get_profile
#' @export
get_profile.conProfile <- function(object,
                                   session = NULL,
                                   what = NULL,
                                   ...) {

    object <- get_profile.distrProfile(object, session, what, ...)
    class(object) <- "conProfile"
    object
}


#' Change the units of the variables in an \code{distrProfile} object
#'
#' @param object An object of class \code{distrProfile} as returned by \code{\link{distributionProfile}}.
#' @param variable A vector of variables to be changed.
#' @param unit A vector with the units, corresponding to variable.
#' @param ... Currently not used.
#' @export
change_units.distrProfile <- function(object,
                                      variable,
                                      unit,
                                      ...) {

    no_variable <- missing(variable)
    no_unit <- missing(unit)

    if (no_unit & no_variable) {
        return(object)
    }
    else {
        units <- get_units(object)
        current <- collect_units(units, unit_reference_sport = attr(object, "unit_reference_sport"))
        p <- length(variable)

        if (length(unit) == p) {
            ## change units
            for (i in variable) {
                currentUnit <- current$unit[current$variable == i]
                newUnit <- unit[which(variable == i)]
                if (currentUnit != newUnit) {
                    conversion <- match.fun(paste(currentUnit, newUnit, sep = "2"))
                    ## change grid
                    newIndex <- conversion(index(object[[i]]))
                    object[[i]] <- zoo(coredata(object[[i]]), order.by = newIndex)
                    ## change units attribute
                    current$unit[current$variable == i] <- newUnit
                }
            }

            ## update units in units
            for (va in current$variable) {
                units$unit[units$variable == va] <- current$unit[current$variable == va]
            }

            attr(object, "units") <- units
            return(object)

        }
        else {
            stop("variable and unit should have the same length.")
        }
    }
}



#' Change the units of the variables in an \code{\link{conProfile}} object
#'
#' @param object An object of class \code{\link{conProfile}} as returned by \code{\link{concentrationProfile}}.
#' @param variable A vector of variables to be changed.
#' @param unit A vector with the units, corresponding to variable.
#' @param ... Currently not used.
#' @export
change_units.conProfile <- change_units.distrProfile

#' Get the operation settings of an \code{distrProfile} object
#'
#' @param object An object of class \code{distrProfile} as returned by \code{\link{distributionProfile}}.
#' @param ... Currently not used.
#' @export
get_operations.distrProfile <- function(object, ...) {
    attr(object, "operations")
}
