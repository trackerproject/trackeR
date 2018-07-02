#' Generate training concentration profiles.
#'
#' @aliases conProfile
#'
#' @param object An object of class \code{distrProfile} as returned by
#'     \code{\link{distributionProfile}}.
#' @param what The variables for which the concentration profiles
#'     should be computed. Defaults (\code{NULL}) to all variables in
#'     \code{object}.
#' @inheritParams distributionProfile
#' @param ... Currently not used.
#' @return
#'
#' An object of class \code{conProfile}.
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
#' Each \code{conProfile} object has the following attributes:
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
#' dProfile <- distributionProfile(run, what = 'speed', grid = seq(0, 12.5, by = 0.05))
#' cProfile <- concentrationProfile(dProfile)
#' plot(cProfile, smooth = FALSE)
#' plot(cProfile)
#' @export
concentration_profile.distrProfile <- function(object,
                                               session = NULL,
                                               what = NULL,
                                               ...) {

    object <- get_profile(object, session = session, what = what)
    what <- names(object)
    ## select sessions
    nc <- nsessions(object)

    ## get concentration profile
    CP <- list()
    for (i in what) {
        CP[[i]] <- -diff(object[[i]])/diff(index(object[[i]]))
    }

    ## class and return
    operations <- get_operations(object)
    attr(CP, "sport") <- get_sport(object)
    attr(CP, "session_times") <- attr(object, "session_times")
    attr(CP, "unit_reference_sport") <- attr(object, "unit_reference_sport")
    attr(CP, "operations") <- operations
    attr(CP, "units") <- get_units(object)
    class(CP) <- "conProfile"
    return(CP)
}

#' Plot concentration profiles.
#'
#' @param x An object of class \code{distrProfile} as returned by
#'     \code{\link{concentration_profile}}.
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
#'
#' @examples
#' data('runs', package = 'trackeR')
#' dProfile <- distributionProfile(runs, session = 1:3, what = 'speed',
#'                                 grid = seq(0, 12.5, by = 0.05))
#' cProfile <- concentrationProfile(dProfile)
#' plot(cProfile, smooth = FALSE)
#' plot(cProfile)
#' @export
plot.conProfile <- function(x,
                            session = NULL,
                            what = NULL,
                            multiple = FALSE,
                            smooth = FALSE,
                            ...) {
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
        ylab(paste0("dtime", " [", duration_unit, "]")) +
        xlab("") +
        theme_bw() +
        scale_colour_continuous(name = "session")
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
#'     \code{\link{concentration_profile}}.
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
#'
#' @seealso \code{\link{smoother_control.distrProfile}}
#' @export
smoother.conProfile <- function(object,
                                session = NULL,
                                what = NULL,
                                control = list(...),
                                ...) {

    object <- get_profile(object, session = session, what = what)

    ## transform to distribution profile
    DP <- list()
    for (i in names(object)) {
        if (is.null(ncol(object[[i]]))) {
            DP[[i]] <- c2d(object[[i]])
        }
        else {
            dp <- matrix(NA, nrow = nrow(object[[i]]), ncol = ncol(object[[i]]))
            colnames(dp) <- attr(object[[i]], "dimnames")[[2]]
            for (j in seq_len(ncol(dp))) {
                dpj <- c2d(object[[i]][, j])
                dp[, j] <- dpj
            }
            DP[[i]] <- zoo(dp, order.by = index(dpj))
        }
    }
    attr(DP, "sport") <- get_sport(object)
    attr(DP, "session_times") <- attr(object, "session_times")
    attr(DP, "unit_reference_sport") <- attr(object, "unit_reference_sport")
    attr(DP, "operations") <- get_operations(object)
    attr(DP, "units") <- get_units(object)
    class(DP) <- "distrProfile"

    ## evaluate control argument
    control <- do.call("smoother_control.distrProfile", control)

    ## smooth distribution profile
    smoothDP <- smoother(DP, control = control)

    ## get concentration profile
    smoothCP <- concentration_profile(smoothDP)

    return(smoothCP)
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
ridges.conProfile <- function(x,
                              session = NULL,
                              what = NULL,
                              smooth = FALSE,
                              ...) {
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
get_sport.conProfile <- function(object,
                                   session = NULL,
                                   ...) {
    if (is.null(session)) {
        nc <- ncol(object[[1]])
        nc <- if (is.null(nc)) 1 else nc
        session <- seq.int(nc)
    }
    attr(object, "sport")[session]
}



#' @rdname concentration_profile.distrProfile
#' @export
concentration_profile.trackeRdata <- function(object,
                                              session = NULL,
                                              what = NULL,
                                              limits = NULL,
                                              parallel = FALSE,
                                              unit_reference_sport = NULL,
                                              scale = FALSE) {
    times <- session_times(object)
    if (is.null(session)) {
        session <- 1:length(object)
    }
    if (is.null(what)) {
        what <- colnames(kantas[[1]])
    }
    object <- object[session]
    if (is.null(limits)) {
        ## Fortify can be extremely slow for large objects...
        limits0 <- compute_limits(object, a = 0.05)
        limits <- lapply(seq.int(nrow(limits0)), function(j) unlist(limits0[j, ]))
        names(limits) <- rownames(limits0)
        for (feature in what) {
            if (all(is.na(limits[[feature]]))) {
                warning(paste('no data for', feature))
                what <- what[!(what %in% feature)]
                limits[[feature]] <- NULL
            }
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
    object <- change_units(object, units$variable, units$unit, units$sport)
    ## check supplied args
    ## if it's a list, it has to either has to be named and contain all element in what or
    ## has to have the same length as what, then it's assumed that the order is the same.
    if (is.list(limits)) {
        if (is.null(names(limits)) & length(what) == length(limits)) {
            names(limits) <- what
        }
        if (is.null(names(limits))) {
            stop("can't match variables in argument 'what' and their limits. Please provide a named list.")
        }
        if (any(is.na(match(what, names(limits))))) {
            stop("please provide a limits for all variables in argument 'what'.")
        }
        limits <- limits[what]
    }
    else {
        if (length(what) == 1L & is.vector(limits)) {
            limits <- list(limits)
            names(limits) <- what
        }
        else {
            stop("arguments 'what' and 'limits' don't match.")
        }
    }
    stopifnot(!any(is.na(match(what, names(limits)))))
    duration_unit <- un$unit[un$variable == "duration"]
    du <- switch(duration_unit, "s" = "secs", "min" = "mins", "h" = "hours", "d" = "days")

    durations <- session_duration(object, duration_unit = duration_unit)

    CP <- NULL

    cp_fun <- function(j, w) {
        sess <- object[[j]]
        values <- sess[, w]
        if (all(is.na(values))) {
            rep(NA, 512)
        }
        else {
            out <- density(values, na.rm = TRUE, from = limits[[w]][1], to = limits[[w]][2], n = 512)$y
            out * ifelse(scale, 1, durations[j])
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
        names(times) <- paste0("session", session)
        CP[[i]] <- times
    }

    ## class and return
    operations <- list(smooth = "density", scale = scale)
    attr(CP, "sport") <- get_sport(object)
    attr(CP, "session_times") <- times[session, ]
    attr(CP, "unit_reference_sport") <- unit_reference_sport
    attr(CP, "operations") <- operations
    attr(CP, "units") <- units
    class(CP) <- "conProfile"
    return(CP)
}
