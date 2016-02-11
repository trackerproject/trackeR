#' Time spent in training zones.
#'
#' @param object An object of class \code{\link{trackeRdata}}.
#' @param session A numeric vector of the sessions to be plotted, defaults to all sessions.
#' @param what A vector of variable names.
#' @param breaks A list of breakpoints between zones, corresponding to the variables in \code{what}.
#' @param parallel Logical. Should computation be carried out in parallel?
#' @param cores Number of cores for parallel computing. If NULL, the number of cores is set to the value of \code{options("corese")} (on Windows) or \code{options("mc.cores")} (elsewhere), or, if the relevant option is unspecified, to half the number of cores detected.
#' @param ... Currently not used.
#' @return An object of class \code{trackeRdataZones}.
#' @seealso \code{\link{plot.trackeRdataZones}}
#' @examples
#' data(run, package = "trackeR")
#' runZones <- zones(run, what = "speed", breaks = list(speed = c(0, 2:6, 12.5)))
#' ## if breaks is a named list, argument 'what' can be left unspecified
#' runZones <- zones(run, breaks = list(speed = c(0, 2:6, 12.5)))
#' ## if only a single variable is to be evaluated, 'breaks' can also be a vector
#' runZones <- zones(run, what = "speed", breaks = c(0, 2:6, 12.5))
#' plot(runZones)
#' @export
zones <- function(object, session = NULL, what = c("speed", "heart.rate"),
                  breaks = list(speed = 0:10, heart.rate = c(0, seq(75, 225, by = 50), 250)),
                  parallel = FALSE, cores = NULL, ...){
    ## select sessions
    if (is.null(session)) session <- seq_along(object)
    object <- object[session]

    ## process zone definitions
    if (!missing(what) && length(what) == 1 & !is.list(breaks)) {
        breaks <- list(breaks)
        names(breaks) <- what
    }
    if (missing(what) & is.null(names(breaks)))
        stop("Variable names need to be provided either in 'what' or the names of 'breaks'.")
    if (missing(what)) what <- names(breaks)
    if (is.null(names(breaks))) names(breaks) <- what

    ## utility function
    zones_for_single_variable <- function(sess, what, breaks){
        dur <- timeAboveThreshold(sess[,"speed"], 0, ge = TRUE) ## use what or speed?
        ta <- sapply(breaks, function(thr) timeAboveThreshold(sess[, what], thr, ge = TRUE))
        td <- -diff(ta)
        perc <- td / as.numeric(dur) * 100

        ## set time spent in zones to minutes
        attr(td, "units") <- units(dur)
        class(td) <- "difftime"
        units(td) <- "mins"
        ##td <- as.numeric(td)

        ret <- data.frame(variable = what, zone = 1:(length(breaks)-1), lower = breaks[-length(breaks)],
                          upper = breaks[-1], time = td, percent = perc)
        return(ret)
    }

    ## get time in zones
    ret <- list()
    if (parallel) {
        dc <- parallel::detectCores()
        if (.Platform$OS.type != "windows"){
            if (is.null(cores))
                cores <- getOption("mc.cores", max(floor(dc/2), 1L))
            for (i in what){
                zonesForVar <- parallel::mclapply(object, zones_for_single_variable,
                                                  what = i, breaks = breaks[[i]], mc.cores = cores)
                zonesForVar <- do.call("rbind", zonesForVar)
                ret[[i]] <- data.frame(session = rep(session, each = length(breaks[[i]]) - 1), zonesForVar)
                rownames(ret[[i]]) <- NULL
            }
        } else {
            if (is.null(cores))
                cores <- getOption("cores", max(floor(dc/2), 1L))
            cl <- parallel::makePSOCKcluster(rep("localhost", cores))
            for (i in what){
                zonesForVar <- parallel::parLapply(cl, object, zones_for_single_variable,
                                                   what = i, breaks = breaks[[i]])
                zonesForVar <- do.call("rbind", zonesForVar)
                ret[[i]] <- data.frame(session = rep(session, each = length(breaks[[i]]) - 1), zonesForVar)
                rownames(ret[[i]]) <- NULL
            }
            parallel::stopCluster(cl)
        }
    } else {
        for (i in what){
            zonesForVar <- lapply(object, zones_for_single_variable, what = i, breaks = breaks[[i]])
            zonesForVar <- do.call("rbind", zonesForVar)
            ret[[i]] <- data.frame(session = rep(session, each = length(breaks[[i]]) - 1), zonesForVar)
            rownames(ret[[i]]) <- NULL
        }
    }

    ## ret <- do.call("rbind", ret);     rownames(ret) <- NULL
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
#' data(run, package = "trackeR")
#' runZones <- zones(run, what = "speed", breaks = c(0, 2:6, 12.5))
#' plot(runZones, percent = FALSE)
#' @export
plot.trackeRdataZones <- function(x, percent = TRUE, ...){

    dat <- do.call("rbind", x)
    dat$zoneF <- factor(paste0("[", paste(dat$lower, dat$upper, sep = "-"), ")"),
                        levels = unique(paste0("[", paste(dat$lower, dat$upper, sep = "-"), ")")))
    dat$session <- factor(dat$session)
    dat$timeN <- as.numeric(dat$time)

    ## basic plot
    p <- ggplot2::ggplot(dat) + ggplot2::xlab("Zones")

    ## y: time or percent
    if (percent) {
        p <- p + ggplot2::geom_bar(ggplot2::aes_(x = quote(zoneF), y = quote(percent), fill = quote(session)),
                                   stat = "identity", position = ggplot2::position_dodge()) +
            ggplot2::ylab("Percent") +
            ggplot2::guides(fill = ggplot2::guide_legend(title = "Session"))
    } else {
        p <- p + ggplot2::geom_bar(ggplot2::aes_(x = quote(zoneF), y = quote(timeN),
                                                 fill = quote(session)),
                                   stat = "identity", position = ggplot2::position_dodge()) +
            ggplot2::ylab(paste0("Time [", units(dat$time), "]")) +
            ggplot2::guides(fill = ggplot2::guide_legend(title = "Session"))
    }

    ## facets
    units <- getUnits(x)
    lab_data <- function(series){
        thisunit <- units$unit[units$variable == series]
        prettyUnit <- prettifyUnits(thisunit)
        paste0(series, " [", prettyUnit,"]")
    }
    lab_data <- Vectorize(lab_data)

    p <-  p + ggplot2::facet_grid(. ~ variable, scales = "free_x",
                                  labeller = ggplot2::labeller("variable" = lab_data))

    ## theme
    p <- p + ggplot2::theme_bw()

    return(p)
}

