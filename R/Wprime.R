#' W' expended.
#'
#' Calculate W' expended, i.e., the work capacity above critical
#' power/speed which has been depleted and not yet been replenished.
#'
#' @param object Univariate \code{\link[zoo]{zoo}} object containing
#'     the time stamped power output or speed values. (Power should be
#'     in Watts, speed in meters per second.)
#' @param w0 Inital capacity of W', as calculated based on the
#'     critical power model by Monod and Scherrer (1965).
#' @param cp Critical power/speed, i.e., the power/speed which can be
#'     maintained for longer period of time.
#' @param version How should W' be replenished? Options include
#'     \code{'2015'} and \code{'2012'} for the versions presented in
#'     Skiba et al. (2015) and Skiba et al. (2012), respectively. See
#'     Details.
#' @param meanRecoveryPower Should the mean of all power outputs below
#'     critical power be used as recovery power? See Details.
#'
#' @details
#'
#' Skiba et al. (2015) and Skiba et al. (2012) both describe an
#' exponential decay of \eqn{W'} expended over an interval
#' \eqn{[t_{i-1}, t_i)} if the power output during this interval is
#' below critical power:
#'
#' \deqn{W_exp (t_i) = W_exp(t_{i-1}) * exp(nu * (t_i - t_{i-1}))}
#'
#' However, the factor nu differs: Skiba et al. (2012) describe it as
#' \eqn{1/\tau} with \eqn{\tau} estimated as
#'
#' \deqn{tau = 546 * exp(-0.01 * (CP - P_i)) + 316}
#'
#' Skiba et al. (2015) use \eqn{(P_i - CP) / W'_0}.  Skiba et
#' al. (2012) and Skiba et al. (2015) employ a constant recovery power
#' (calculated as the mean over all power outputs below critical
#' power). This rationale can be applied by setting the argument
#' \code{meanRecoveryPower} to \code{TRUE}. Note that this uses
#' information from all observations with a power output below
#' critical power, not just those prior to the current time point.
#'
#' @references
#'
#' Monod H, Scherrer J (1965). 'The Work Capacity of a Synergic
#' Muscular Group.' Ergonomics, 8(3), 329--338.
#'
#' Skiba PF, Chidnok W, Vanhatalo A, Jones AM (2012). 'Modeling the
#' Expenditure and Reconstitution of Work Capacity above Critical
#' Power.' Medicine & Science in Sports & Exercise, 44(8), 1526--1532.
#'
#' Skiba PF, Fulford J, Clarke DC, Vanhatalo A, Jones AM
#' (2015). 'Intramuscular Determinants of the Abilility to Recover
#' Work Capacity above Critical Power.' European Journal of Applied
#' Physiology, 115(4), 703--713.
#'
#' @export
Wexp <- function(object, w0, cp, version = c("2015", "2012"), meanRecoveryPower = FALSE) {
    version <- match.arg(version, c("2015", "2012"))

    time <- index(object)
    power <- coredata(object)

    dp <- power - cp
    delta <- as.numeric(difftime(c(time[-1], time[length(time)]), time, units = "secs"))
    ind <- dp > 0

    wna <- is.na(power)
    ## if all power values are NA, return NA
    if (all(wna)) {
        ret <- cbind(coredata(object), power)
        colnames(ret) <- c("movement", "wprime")
        ret <- zoo(ret, order.by = index(object))
        return(ret)
    }
    ## otherwise carry on
    time <- time[!wna]
    power <- power[!wna]
    dp <- dp[!wna]
    delta <- delta[!wna]
    ind <- ind[!wna]

    ## Done in papers (Skiba et al (2012) and Skiba et al (2015)) but not necessary with
    ## assumption of constant power over intervals between observations: use mean power over
    ## all instances with power below CP as recovery power (here weighted by length of time
    ## interval)
    if (meanRecoveryPower) {
        recovery <- power <= cp
        rPower <- power[recovery]
        rDelta <- delta[recovery]
        rDeltaSum <- sum(rDelta)
        dp[recovery] <- sum(rPower * rDelta/sum(rDeltaSum)) - cp
    }

    ## recovery factor
    if (version == "2015") {
        recoveryFactor <- dp/w0
    }
    else {
        ## version 2012
        tau <- 546 * exp(0.01 * dp) + 316
        recoveryFactor <- -1/tau
    }

    ## W'expended is W' previously depleted and not yet recovered
    Wexp <- rep(NA, length(ind))
    Wexp[1] <- if (ind[1])
        dp[1] * delta[1] else 0
    for (i in 2:length(ind)) {
        if (ind[i]) {
            Wexp[i] <- Wexp[i - 1] + dp[i] * delta[i]
        } else {
            Wexp[i] <- Wexp[i - 1] * exp(recoveryFactor[i] * delta[i])
        }
    }

    ret <- rep(NA, length(object))
    ret[!wna] <- Wexp
    ret <- cbind(coredata(object), ret)
    colnames(ret) <- c("movement", "wprime")
    ret <- zoo(ret, order.by = index(object))

    return(ret)
}

#' W': work capacity above critical power/speed.
#'
#' Based on the critical power model for cycling (Monod and Scherrer,
#' 1965), W' (read W prime) describes the finite work capacity above
#' critical power (Skiba et al., 2012).  While W' is depleted during
#' exercise above critical power, it is replenished during exercise
#' below critical power. Thus, it is of interest how much of this work
#' capacity has been depleted and not yet been replinished again,
#' named W' expended, or how much of this work capacity is still
#' available, named W' balance.  This principal is applied to runners
#' by subsituting power and critical power with speed and critical
#' speed, respectively (Skiba et al., 2012).
#'
#' @param object A \code{\link{trackeRdata}} object.
#' @param session A numeric vector of the sessions to be used,
#'     defaults to all sessions.
#' @param quantity Should W' \code{'expended'} or W' \code{'balance'}
#'     be returned?
#' @inheritParams Wexp
#' @param parallel Logical. Should computation be carried out in
#'     parallel? If \code{TRUE} computation is performed in parallel
#'     using the backend provided to \code{\link{foreach}}. Default is
#'     \code{FALSE}.
#' @param ... Currently not used.
#'
#' @return An object of class \code{trackeRWprime}.
#'
#' @details
#'
#' #' Skiba et al. (2015) and Skiba et al. (2012) both describe an
#' exponential decay of \eqn{W'} expended over an interval
#' \eqn{[t_{i-1}, t_i)} if the power output during this interval is
#' below critical power:
#'
#' \deqn{W_exp (t_i) = W_exp(t_{i-1}) * exp(nu * (t_i - t_{i-1}))}
#'
#' However, the factor nu differs: Skiba et al. (2012) describe it as
#' \eqn{1/\tau} with \eqn{\tau} estimated as
#'
#' \deqn{tau = 546 * exp(-0.01 * (CP - P_i)) + 316}
#'
#' Skiba et al. (2015) use \eqn{(P_i - CP) / W'_0}.  Skiba et
#' al. (2012) and Skiba et al. (2015) employ a constant recovery power
#' (calculated as the mean over all power outputs below critical
#' power). This rationale can be applied by setting the argument
#' \code{meanRecoveryPower} to \code{TRUE}. Note that this uses
#' information from all observations with a power output below
#' critical power, not just those prior to the current time point.
#'
#' @references
#'
#' Monod H, Scherrer J (1965). 'The Work Capacity of a Synergic
#' Muscular Group.' Ergonomics, 8(3), 329--338.
#'
#' Skiba PF, Chidnok W, Vanhatalo A, Jones AM (2012). 'Modeling the
#' Expenditure and Reconstitution of Work Capacity above Critical
#' Power.' Medicine & Science in Sports & Exercise, 44(8), 1526--1532.
#'
#' Skiba PF, Fulford J, Clarke DC, Vanhatalo A, Jones AM
#' (2015). 'Intramuscular Determinants of the Abilility to Recover
#' Work Capacity above Critical Power.' European Journal of Applied
#' Physiology, 115(4), 703--713.
#'
#' @export
#' @examples
#' data('runs', package = 'trackeR')
#' wexp <- Wprime(runs, session = c(11,13), cp = 4, version = '2012')
#' plot(wexp)
Wprime <- function(object, session = NULL, quantity = c("expended", "balance"), w0, cp,
    version = c("2015", "2012"), meanRecoveryPower = FALSE, parallel = FALSE,
    ...) {
    ## prep args
    quantity <- match.arg(quantity, c("expended", "balance"))
    if (quantity == "balance" & missing(w0))
        stop("Please provide w0 or choose quantity = 'expended'.")
    version <- match.arg(version, c("2015", "2012"))
    if (version == "2015" & missing(w0))
        stop("Please provide w0 or choose version = '2012'.")
    if (missing(w0))
        w0 <- NA

    ## units
    units <- getUnits(object)

    ## select sessions
    if (is.null(session))
        session <- 1:length(object)
    object <- object[session]
    sports <- sport(object)

    ## FIXME: change here to accommodate multisport environment for
    ## now, do not allow computation of Wprime if user does not
    ## understand what they are trying to do
    if (sum(c("running", "cycling") %in% na.omit(unique(sports))) != 1) {
        stop("Wprime applies only for running or only for cycling sessions")
    }

    cycling <- units$unit[units$variable == "cadence"] == "rev_per_min"
    ps <- ifelse(cycling, "power", "speed")
    if (cycling) {
        if (units$unit[units$variable == "power"] != "W") {
            object <- change_units(object, variable = "power", unit = "W")
            units <- getUnits(object)
            conversion <- match.fun(paste(units$unit[units$variable == "power"], "W", sep = "2"))
            cp <- conversion(cp)
        }
    }
    else {
        if (units$unit[units$variable == "speed"] != "m_per_s") {
            object <- change_units(object, variable = "speed", unit = "m_per_s")
            units <- getUnits(object)
            conversion <- match.fun(paste(units$unit[units$variable == "speed"], "m_per_s",
                sep = "2"))
            cp <- conversion(cp)
        }
    }


    ## get W'
    getW <- function(j) {
        sess <- object[[j]]
        W <- Wexp(sess[, ps], w0 = w0, cp = cp, version = version, meanRecoveryPower = meanRecoveryPower)
        if (quantity == "balance")
            W$wprime <- w0 - W$wprime
        return(W)
    }

    foreach_object <- eval(as.call(c(list(quote(foreach::foreach),
                                          j = seq.int(nsessions(object))))))

    if (parallel) {
        setup_parallel()
        ret <- foreach::`%dopar%`(foreach_object, getW(j))
    }
    else {
        ret <- foreach::`%do%`(foreach_object, getW(j))
    }

    ## class and return
    attr(ret, "quantity") <- quantity
    attr(ret, "w0") <- if (missing(w0))
        NA else w0
    attr(ret, "cp") <- cp

    ## FIXME: fixme here to accommodate for multi-sport environment
    attr(ret, "cycling") <- cycling
    attr(ret, "sport") <- sports
    attr(ret, "unit") <- units[units$variable == ps, ]
    class(ret) <- "trackeRWprime"
    return(ret)
}

#' Plot W'.
#'
#' @param x An object of class \code{trackeRWprime} as returned by \code{\link{Wprime}}.
#' @param session A numeric vector of the sessions to be plotted, defaults to all sessions.
#' @param dates Logical. Should the date of the session be used in the panel header?
#' @param scaled Logical. Should the W' be scaled to the movement variable (power or speed)
#'     which is then plotted in the background?
#' @param ... Currently not used.
#' @export
#' @examples
#' data('runs', package = 'trackeR')
#' wexp <- Wprime(runs, session = 1:3, cp = 4, version = '2012')
#' plot(wexp, session = 1:2)
plot.trackeRWprime <- function(x, session = NULL, dates = TRUE, scaled = TRUE, ...) {
    quantity <- attr(x, "quantity")
    sports <- attr(x, "sport")
    cp <- attr(x, "cp")
    cycling <- attr(x, "cycling")
    Wunit <- if (cycling) "[J]" else "[m]"
    mylabels <- c(paste0(ifelse(cycling, "Power", "Speed"),
                         " [",
                         prettifyUnits(attr(x, "unit")$unit), "]"),
                  paste("W'", quantity, "[scaled]"))

    ## select sessions
    if (is.null(session))
        session <- seq_along(x)

    x <- x[session]

    ## transform W' to match power/speed scale
    if (scaled) {
        sdMov <- stats::sd(unlist(lapply(x, function(z) z$movement)), na.rm = TRUE)
        mMov <- mean(unlist(lapply(x, function(z) z$movement)), na.rm = TRUE)

        x <- lapply(x, function(z) {
            w <- (coredata(z$wprime) - mean(coredata(z$wprime), na.rm = TRUE))/stats::sd(coredata(z$wprime),
                na.rm = TRUE)
            w <- w * sdMov  #sd(coredata(z$movement), na.rm = TRUE)
            z$wprime <- w + mMov
            # max(mMov, abs(min(w, na.rm = TRUE))) max(mean(coredata(z$movement), na.rm = TRUE),
            # abs(min(w, na.rm = TRUE)))
            return(z)
        })
    }

    ## class and get data
    attr(x, "sport") <- sports[session]
    class(x) <- "trackeRWprime"
    df <- fortify(x, melt = TRUE)



    ## prepare session id for panel header
    if (dates) {
        df$SessionID <- format(session[df$SessionID])
        df$SessionID <- gsub(" ", "0", df$SessionID)
        df$SessionID <- paste0(paste(df$SessionID, df$Sport, sep = ": "), "\n", format(df$Index, "%Y-%m-%d"))
    }
    else {
        df$SessionID <- paste0(paste(df$SessionID, df$Sport, sep = ": "))
        ## factor(df$SessionID, levels = seq_along(session), labels = session)
    }
    df$Series <- factor(df$Series)

    ## check that there is data to plot
    for (l in levels(df$Series)) {
        if (all(is.na(subset(df, Series == l, select = "Value"))))
            df <- df[!(df$Series == l), ]
    }

    ## make facets
    singleSession <- length(session) == 1L
    facets <- if (singleSession)
        NULL else ". ~ SessionID"

    if (scaled) {
        ## basic plot
        p <- ggplot(data = df, mapping = aes_(x = quote(Index), y = quote(Value))) +
            ylab("") + xlab("Time")
        ## lines for power/speed and W'
        p <- p + geom_line(aes_(group = quote(Series), col = quote(Series)),
            na.rm = TRUE) + scale_colour_manual(name = "", labels = mylabels,
            values = c("gray", "blue"))
        ## add line for cp
        p <- p + geom_hline(data = data.frame(cp = cp), aes(yintercept = cp),
            col = "black")
    } else {
        ## basic plot
        p <- ggplot(data = subset(df, Series == "wprime"), mapping = aes_(x = quote(Index),
            y = quote(Value))) + ylab(paste("W'", quantity, Wunit)) + xlab("Time")
        ## lines for W'
        p <- p + geom_line(na.rm = TRUE)
    }
    ## add facet if necessary
    if (!is.null(facets)) {
        p <- p + facet_grid(facets, scales = "free")
    }
    ## add bw theme
    p <- p + theme_bw() + theme(legend.position = "top", axis.text.x = element_text(angle = 50,
        hjust = 1))

    return(p)
}


#' Fortify a trackeRWprime object for plotting with ggplot2.
#'
#' @param model The \code{trackeRWprime} object as returned by \code{\link{Wprime}}.
#' @param data Ignored.
#' @param melt Logical. Should the data be melted into long format
#'     instead of the default wide format?
#' @param ... Ignored.
#' @export
fortify.trackeRWprime <- function(model, data, melt = FALSE, ...) {
    ret <- list()
    sports <- sport(model)
    for (i in seq_along(model)) {
        ret[[i]] <- zoo::fortify.zoo(model[[i]], melt = melt)
        ret[[i]]$SessionID <- i
        ret[[i]]$Sport <- sports[i]
    }
    ret <- do.call("rbind", ret)
    return(ret)
}

#' @rdname nsessions
#' @export
nsessions.trackeRWprime <- function(object, ...) {
    length(object)
}


#' @rdname sport
#' @export
sport.trackeRWprime <- function(object, ...) {
    attr(object, "sport")
}
