#' W' expended.
#'
#' Calculate W' expended, i.e., the work capacity above critical power
#' which has been depleted and not yet been replenished.
#'
#' @param object Univariate \code{\link[zoo]{zoo}} object containing the time stamped power output or speed values. (Power should be in Watts, speed in meters per second.)
#' @param w0 Inital capacity of W', as calculated based on the critical
#' power model by Monod and Scherrer (1965).
#' @param cp Critical power/speed, i.e., the power/speed which can be maintained for
#' longer period of time. 
#' @param version How should W' be replenished? Options include \code{"2015"}
#' and \code{"2012"} for the versions presented in Skiba et al. (2015) and
#' Skiba et al. (2012), respectively. See Details.
#' @param meanRecoveryPower Should the mean of all power outputs below critical
#' power be used as recovery power? See Details.
#' 
#' @details Skiba et al. (2015) and Skiba et al. (2012) both describe an exponential decay of W' expended over an interval [t_{i-1}, t_i) if the power output during this interval is below critical power:
#' W_exp (t_i) = W_exp(t_{i-1}) * exp(nu * (t_i - t_{i-1})).
#' However, the factor nu differs: Skiba et al. (2012) describe it as 1/tau with tau estimated as
#' tau = 546 * exp( -0.01 * (CP - P_i) + 316.
#' Skiba et al. (2015) use (P_i - CP) / W'_0.
#' Skiba et al. (2012) and Skiba et al. (2015) employ a constant recovery power (calculated as the mean over all power outputs below critical power). This rational can be applied by setting the argument \code{meanRecoveryPower} to \code{TRUE}. Note that this employes information from the all observations with a power output below critical power, not just those prior to the current time point.
#'
#' @references Monod H, Scherrer J (1965). "The Work Capacity of a Synergic Muscular Group." Ergonomics, 8(3), 329--338.
#' Skiba PF, Chidnok W, Vanhatalo A, Jones AM (2012). "Modeling the Expenditure and Reconstitution of Work Capacity above Critical Power." Medicine & Science in Sports & Exercise, 44(8), 1526--1532.
#' Skiba PF, Fulford J, Clarke DC, Vanhatalo A, Jones AM (2015). "Intramuscular Determinants of the Abilility to Recover Work Capacity above Critical Power." European Journal of Applied Physiology, 115(4), 703--713.
Wexp <- function(object, w0, cp, 
                 version = c("2015", "2012"), meanRecoveryPower = FALSE){
    version <- match.arg(version, c("2015", "2012"))

    time <- index(object)
    power <- coredata(object)
    
    dp <- power - cp
    delta <- as.numeric(difftime(c(time[-1], time[length(time)]), time, units = "secs"))
    ind <- dp > 0

    wna <- is.na(power)
    time <- time[!wna]
    power <- power[!wna]
    dp <- dp[!wna]
    delta <- delta[!wna]
    ind <- ind[!wna]
    
    ## Done in papers (Skiba et al (2012) and Skiba et al (2015)) but not necessary
    ## with assumption of constant power over intervals between observations:
    ## use mean power over all instances with power below CP as recovery power
    ## (here weighted by length of time interval)
    if (meanRecoveryPower) {
        recovery <- power <= cp
        rPower <- power[recovery]
        rDelta <- delta[recovery]
        rDeltaSum <- sum(rDelta)
        dp[recovery] <- sum(rPower * rDelta/sum(rDeltaSum)) - cp
    }
    
    ## recovery factor
    if (version == "2015") {
        recoveryFactor <- dp / w0
    } else { ## version 2012
        tau <- 546 * exp(0.01 * dp) + 316
        recoveryFactor <- - 1/tau
    }
    
    ## W'expended is W' previously depleted and not yet recovered
    Wexp <- rep(NA, length(ind))
    Wexp[1] <- if (ind[1]) dp[1]*delta[1] else 0
    for (i in 2:length(ind)){
        if (ind[i]) {
            Wexp[i] <- Wexp[i-1] + dp[i]*delta[i]
        } else {
            Wexp[i] <- Wexp[i-1] * exp(recoveryFactor[i] * delta[i])
        }
    }

    ret <- rep(NA, length(object))
    ret[!wna] <- Wexp
    ret <- cbind(coredata(object), ret)
    colnames(ret) <- c("movement", "wprime")
    ret <- zoo(ret, order.by = index(object))
    
    return(ret)
}

#' W': work capacity above critical power.
#'
#' Based on the critical power model for cycling (Monod and Scherrer, 1965),
#' W' (read W prime) describes the finite work capacity above critical power (Skiba et al., 2012).
#' While W' is depleted during exercise above critical power, it is replenished
#' during exercise below critical power. Thus, it is of interest how much of this
#' work capacity has been depleted and not yet been replinished again, named
#' W' expended, or how much of this work capacity is still available, named W' balance.
#' This principal is applied to runners by subsituting power and critical power with
#' speed and critical speed, respectively (Skiba et al., 2012).
#'
#' @param object A \code{\link{trackeRdata}} object.
#' @param session A numeric vector of the sessions to be used, defaults to all sessions.
#' @param quantity Should W' \code{"expended"} or W' \code{"balance"} be returned?
#' @inheritParams Wexp
#' @param parallel Logical. Should computation be carried out in parallel?
#' @param cores Number of cores for parallel computing. If NULL, the number of cores is set to the value of \code{options("corese")} (on Windows) or \code{options("mc.cores")} (elsewhere), or, if the relevant option is unspecified, to half the number of cores detected.
#' @param ... Currently not used.
#'
#' @return An object of class \code{trackeRWprime}.
#'
#' @details Skiba et al. (2015) and Skiba et al. (2012) both describe an exponential decay of
#' W' expended over an interval [t_{i-1}, t_i) if the power output during this interval is below critical power:
#' W_exp (t_i) = W_exp(t_{i-1}) * exp(nu * (t_i - t_{i-1})).
#' However, the factor nu differs: Skiba et al. (2012) describe it as 1/tau with tau estimated as
#' tau = 546 * exp( -0.01 * (CP - P_i) + 316.
#' Skiba et al. (2015) use (P_i - CP) / W'_0.
#' Skiba et al. (2012) and Skiba et al. (2015) employ a constant recovery power (calculated as the mean
#' over all power outputs below critical power). This rational can be applied by setting the argument
#' \code{meanRecoveryPower} to \code{TRUE}. Note that this employes information from the all observations
#' with a power output below critical power, not just those prior to the current time point.
#'
#' @references Monod H, Scherrer J (1965). "The Work Capacity of a Synergic Muscular Group." Ergonomics, 8(3), 329--338.
#' Skiba PF, Chidnok W, Vanhatalo A, Jones AM (2012). "Modeling the Expenditure and Reconstitution of Work Capacity above Critical Power." Medicine & Science in Sports & Exercise, 44(8), 1526--1532.
#' Skiba PF, Fulford J, Clarke DC, Vanhatalo A, Jones AM (2015). "Intramuscular Determinants of the Abilility to Recover Work Capacity above Critical Power." European Journal of Applied Physiology, 115(4), 703--713.
#'
#' @export
#' @examples
#' data("runs", package = "trackeR")
#' wexp <- Wprime(runs, session = 11:13, cp = 4, version = "2012")
#' plot(wexp)
Wprime <- function(object, session = NULL, quantity = c("expended", "balance"),
                   w0, cp, version = c("2015", "2012"), meanRecoveryPower = FALSE,
                   parallel = FALSE, cores = NULL, ...){
    ## prep args
    quantity <- match.arg(quantity, c("expended", "balance"))
    if (quantity == "balance" & missing(w0)) stop("Please provide w0 or choose quantity = 'expended'.")
    version <- match.arg(version, c("2015", "2012"))
    if (version == "2015" & missing(w0)) stop("Please provide w0 or choose version = '2012'.")
    if (missing(w0)) w0 <- NA

    ## units
    units <- getUnits(object)
    cycling <- units$unit[units$variable == "cadence"] == "rev_per_min"
    ps <- ifelse(cycling, "power", "speed")
    if (cycling) {
        if (units$unit[units$variable == "power"] != "W"){
            changeUnits(object, variable = "power", unit = "W")
            conversion <- match.fun(paste(units$unit[units$variable == "power"], "W", sep = "2"))
            cp <- conversion(cp)
            units$unit[units$variable == "power"] <- "W"
        }
    } else {
        if (units$unit[units$variable == "speed"] != "m_per_s"){
            changeUnits(object, variable = "speed", unit = "m_per_s")
            conversion <- match.fun(paste(units$unit[units$variable == "speed"], "m_per_s", sep = "2"))
            cp <- conversion(cp)
            units$unit[units$variable == "speed"] <- "m_per_s"
        }
    }

    ## select sessions
    if (is.null(session)) session <- 1:length(object)
    object <- object[session]

    ## get W'
    getW <- function(x){
        W <- Wexp(x[,ps], w0 = w0, cp = cp, version = version,
                  meanRecoveryPower = meanRecoveryPower)
        if (quantity == "balance") W$wprime <- w0 - W$wprime
        return(W)
    }

    if (parallel) {
        dc <- parallel::detectCores()
        if (.Platform$OS.type != "windows"){
            if (is.null(cores))
                cores <- getOption("mc.cores", max(floor(dc/2), 1L))
            ## parallel::mclapply(...,  mc.cores = cores)
            ret <- parallel::mclapply(object, getW, mc.cores = cores)
        } else {
            if (is.null(cores))
                cores <- getOption("cores", max(floor(dc/2), 1L))
            cl <- parallel::makePSOCKcluster(rep("localhost", cores))
            ## parallel::parLapply(cl, ...)
            ret <- parallel::parLapply(cl, object, getW)
            parallel::stopCluster(cl)
        }
    } else {
        ## lapply(...)
        ret <- lapply(object, getW)
    }

    ## class and return
    attr(ret, "quantity") <- quantity
    attr(ret, "w0") <- if (missing(w0)) NA else w0
    attr(ret, "cp") <- cp
    attr(ret, "cycling") <- cycling
    attr(ret, "unit") <- units[units$variable == ps,]
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
#' data("runs", package = "trackeR")
#' wexp <- Wprime(runs, session = 1:3, cp = 4, version = "2012")
#' plot(wexp, session = 1:2) 
plot.trackeRWprime <- function(x, session = NULL, dates = TRUE, scaled = TRUE, ...){
    ## the following line is just intended to prevent R CMD check to produce the NOTE
    ## "no visible binding for global variable 'Series'" because that variable is used in subset()
    Series <- NULL

    quantity <- attr(x, "quantity")
    cp <- attr(x, "cp")
    cycling <- attr(x, "cycling")
    Wunit <- if (cycling) "[J]" else "[m]"
    mylabels <- c(paste0(ifelse(cycling, "Power", "Speed"), " [", prettifyUnits(attr(x, "unit")$unit), "]"),
                  paste("W'", quantity, "[scaled]"))
    
    ## select sessions
    if (is.null(session)) session <- seq_along(x)
    x <- x[session]

    ## transform W' to match power/speed scale
    if (scaled){
        sdMov <- stats::sd(unlist(lapply(x, function(z) z$movement)), na.rm = TRUE)
        mMov <- mean(unlist(lapply(x, function(z) z$movement)), na.rm = TRUE)

        x <- lapply(x, function(z){
                        w <- (coredata(z$wprime) - mean(coredata(z$wprime), na.rm = TRUE)) /
                            stats::sd(coredata(z$wprime), na.rm = TRUE)
                        w <- w * sdMov #sd(coredata(z$movement), na.rm = TRUE)
                        z$wprime <- w + mMov
                                        # max(mMov, abs(min(w, na.rm = TRUE)))
                                        # max(mean(coredata(z$movement), na.rm = TRUE), abs(min(w, na.rm = TRUE)))
                        return(z)
                    })
    }

    ## get data
    class(x) <- "trackeRWprime"
    df <- fortify(x, melt = TRUE)

    ## prepare session id for panel header 
    if (dates) {
        df$SessionID <- format(session[df$SessionID])
        df$SessionID <- gsub(" ", "0", df$SessionID)
        df$SessionID <- paste(df$SessionID, format(df$Index, "%Y-%m-%d"), sep = ": ")
    } else {
        df$SessionID <- factor(df$SessionID, levels = seq_along(session), labels = session)
    }
    df$Series <- factor(df$Series)

    ## check that there is data to plot
    for(l in levels(df$Series)){
        if (all(is.na(subset(df, Series == l, select = "Value"))))
            df <- df[!(df$Series == l), ]
    }

    ## make facets
    singleSession <- length(session) == 1L
    facets <- if (singleSession) NULL else ". ~ SessionID"

    if (scaled){
        ## basic plot
        p <- ggplot2::ggplot(data = df, mapping = ggplot2::aes_(x = quote(Index), y = quote(Value))) +
            ggplot2::ylab("") + ggplot2::xlab("Time")
        ## lines for power/speed and W'
        p <- p + ggplot2::geom_line(ggplot2::aes_(group = quote(Series), col = quote(Series))) +
            ggplot2::scale_colour_manual(name = "", labels = mylabels, values = c("gray","blue"))
        ## add line for cp
        p <- p + ggplot2::geom_hline(data = data.frame(cp = cp), ggplot2::aes(yintercept = cp), col = "black")
    } else {
        ## basic plot
        p <- ggplot2::ggplot(data = subset(df, Series == "wprime"),
                             mapping = ggplot2::aes_(x = quote(Index), y = quote(Value))) +
            ggplot2::ylab(paste("W'", quantity, Wunit)) + ggplot2::xlab("Time")
        ## lines for W'
        p <- p + ggplot2::geom_line()        
    }
    ## add facet if necessary
    if (!is.null(facets)){
        p <- p + ggplot2::facet_grid(facets, scales = "free")
    }
    ## add bw theme
    p <- p + ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 50, hjust = 1))

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
fortify.trackeRWprime <- function(model, data, melt = FALSE, ...){
    ret <- list()
    for (i in seq_along(model)){
        ret[[i]] <- zoo::fortify.zoo(model[[i]], melt = melt)
        ret[[i]]$SessionID <- i
    }
    ret <- do.call("rbind", ret)
    return(ret)
}
