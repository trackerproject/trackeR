#' Time spent in training zones.
#'
#' @param object An object of class \code{\link{trackeRdata}}.
#' @param session A numeric vector of the sessions to be plotted, defaults to all sessions.
#' @param what A vector of variable names.
#' @param breaks A list of breakpoints between zones, corresponding to the variables in \code{what}.
#' @param parallel Logical. Should computation be carried out in parallel?
#' @param cores Number of cores for parallel computing. If NULL, the number of cores is set to the value of \code{options('corese')} (on Windows) or \code{options('mc.cores')} (elsewhere), or, if the relevant option is unspecified, to half the number of cores detected.
#' @param auto_breaks Logical. Should breaks be selected automatically? Default is \code{FALSE} and \code{breaks} will be ignored if \code{TRUE}.
#' @param n_zones A numeric. If auto_breaks=TRUE, select number of zones for data to be split into.
#' @param ... Currently not used.
#' @return An object of class \code{trackeRdataZones}.
#' @seealso \code{\link{plot.trackeRdataZones}}
#' @examples
#' data('run', package = 'trackeR')
#' runZones <- zones(run, what = 'speed', breaks = list(speed = c(0, 2:6, 12.5)))
#' ## if breaks is a named list, argument 'what' can be left unspecified
#' runZones <- zones(run, breaks = list(speed = c(0, 2:6, 12.5)))
#' ## if only a single variable is to be evaluated, 'breaks' can also be a vector
#' runZones <- zones(run, what = 'speed', breaks = c(0, 2:6, 12.5))
#' plot(runZones)
#' @export
zones <- function(object, session = NULL, what = c("speed", "heart.rate"), breaks = list(speed = 0:10,
    heart.rate = c(0, seq(75, 225, by = 50), 250)), parallel = FALSE, cores = NULL, auto_breaks = TRUE,
    n_zones = 9, ...) {

    ## select sessions
    if (is.null(session)) {
        session <- seq_along(object)
    }
    object <- object[session]


    if (auto_breaks) {
        breaks <- list()

        df <- fortify(object, melt = FALSE)

        find_step_size <- function (maximum, minimum = 0) {
            value_range <- as.character(ceiling(maximum - minimum))
            range_size <- nchar(value_range)
            round_table <- list('1' = 5, '2' = 5, '3' = 10, '4' = 100,
                                '5' = 10000, '6' = 100000)
            maximum <- ceiling(maximum/round_table[[range_size]]) * round_table[[range_size]]
            step_size <- round((maximum - minimum) / (n_zones), 1)
            break_points <- seq(minimum, minimum + n_zones * step_size, by = step_size)
            break_points
        }

        for (feature in what) {
            if (all(is.na(df[[feature]]))) {
                warning(paste('No data for', feature))
                what <- what[!(what %in% feature)]
            }
        }
        for (feature in what) {
            maximum <- ceiling(quantile(df[feature], 0.98, na.rm = TRUE))
            minimum <- if (feature == 'heart.rate') 60 else floor(quantile(df[feature], 0.01, na.rm = TRUE))
            breaks[[feature]] <- find_step_size(maximum, minimum)
        }
    }

    ## process zone definitions
    if (!missing(what) && length(what) == 1 & !is.list(breaks)) {
        breaks <- list(breaks)
        names(breaks) <- what
    }
    if (missing(what) & is.null(names(breaks)))
        stop("Variable names need to be provided either in 'what' or the names of 'breaks'.")
    if (missing(what))
        what <- names(breaks)
    if (is.null(names(breaks)))
        names(breaks) <- what

    ## utility function
    zones_for_single_variable <- function(sess, what, breaks) {
        dur <- timeAboveThreshold(sess[, "speed"], 0, ge = TRUE)  ## use what or speed?
        ta <- sapply(breaks, function(thr) timeAboveThreshold(sess[, what], thr, ge = TRUE))
        td <- -diff(ta)
        perc <- td/as.numeric(dur) * 100

        ## set time spent in zones to minutes
        attr(td, "units") <- units(dur)
        class(td) <- "difftime"
        units(td) <- "mins"
        ## td <- as.numeric(td)

        ret <- data.frame(variable = what, zone = 1:(length(breaks) - 1), lower = breaks[-length(breaks)],
            upper = breaks[-1], time = td, percent = perc)
        return(ret)
    }

    ## get time in zones
    zones_fun <- function(j, w) {
        sess <- object[[j]]
        zones_for_single_variable(sess, what = i, breaks = breaks[[w]])
    }
    ret <- list()
    for (i in what) {
        foreach_object <- eval(as.call(c(list(quote(foreach::foreach),
                                              j = seq.int(nsessions(object)),
                                              .combine = "rbind"))))
        if (parallel) {
            setup_parallel()
            zonesForVar <- foreach::`%dopar%`(foreach_object, zones_fun(j, i))
        }
        else {
            zonesForVar <- foreach::`%do%`(foreach_object, zones_fun(j, i))
        }
        ret[[i]] <- data.frame(session = rep(session,
                                             each = length(breaks[[i]]) - 1),
                               zonesForVar)
        rownames(ret[[i]]) <- NULL
    }

    attr(ret, "units") <- getUnits(object)
    class(ret) <- c("trackeRdataZones", class(ret))
    return(ret)
}

#' Plot training zones.
#'
#' @param x An object of class \code{trackeRdataZones} as returned by \code{\link{zones}}.
#' @param percent Logical. Should the relative or absolute times spent training in the different zones be plotted?
#' @param ... Currently not used.
#' @examples
#' data('run', package = 'trackeR')
#' runZones <- zones(run, what = 'speed', breaks = c(0, 2:6, 12.5))
#' plot(runZones, percent = FALSE)
#' @export
plot.trackeRdataZones <- function(x, percent = TRUE, ...) {

    dat <- do.call("rbind", x)
    dat$zoneF <- factor(paste0("[", paste(dat$lower, dat$upper, sep = "-"), ")"), levels = unique(paste0("[",
        paste(dat$lower, dat$upper, sep = "-"), ")")))
    ## dat$session <- factor(dat$session)
    dat$Session <- dat$session  ## rename for legend title
    dat$timeN <- as.numeric(dat$time)

    ## basic plot
    p <- ggplot2::ggplot(dat) + ggplot2::xlab("Zones")

    ## y: time or percent
    if (percent) {
        p <- p + ggplot2::geom_bar(ggplot2::aes_(x = quote(zoneF), y = quote(percent),
            fill = quote(Session), group = quote(Session)), stat = "identity", position = ggplot2::position_dodge()) +
            ggplot2::ylab("Percent")  ## +
        ## ggplot2::guides(fill = ggplot2::guide_legend(title = 'Session'))
    } else {
        p <- p + ggplot2::geom_bar(ggplot2::aes_(x = quote(zoneF), y = quote(timeN), fill = quote(Session),
            group = quote(Session)), stat = "identity", position = ggplot2::position_dodge()) +
            ggplot2::ylab(paste0("Time [", units(dat$time), "]"))  ## +
        ## ggplot2::guides(fill = ggplot2::guide_legend(title = 'Session'))
    }

    ## set colors hclpal <- colorspace::rainbow_hcl(n = nl  evels(dat$session), c = 60) p <- p
    ## + ggplot2::scale_fill_manual(values = hclpal)

    ## facets
    units <- getUnits(x)
    lab_data <- function(series) {
        thisunit <- units$unit[units$variable == series]
        prettyUnit <- prettifyUnits(thisunit)
        paste0(series, "\n[", prettyUnit, "]")
    }
    lab_data <- Vectorize(lab_data)

    p <- p + ggplot2::facet_grid(. ~ variable, scales = "free_x", labeller = ggplot2::labeller(variable = lab_data))

    ## theme
    p <- p + ggplot2::theme_bw()  +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 50, hjust = 1),
                       panel.grid.major = element_blank(), panel.grid.minor = element_blank())

    return(p)
}

#' @export
nsessions.trackeRdataZones <- function(object, ...) {
    length(unique(object[[1]]$session))
}
