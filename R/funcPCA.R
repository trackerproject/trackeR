#' Transform distribution and concentration profiles to functional data objects of class fd.
#'
#' @param object An object of class \code{distrProfile} or \code{conProfile}, as returned by \code{\link{distributionProfile}} and \code{\link{concentrationProfile}}, respectively.
#' @param what The variable for which the profiles should be transformed to a functional data object.
#' @param ... Additional arguments passed on to \code{\link[fda]{Data2fd}}
#' @return An object of class \code{\link[fda]{fd}}.
#' @examples
#' library('fda')
#' data('runs', package = 'trackeR')
#' dp <- distributionProfile(runs, what = 'speed')
#' dpFun <- profile2fd(dp, what = 'speed',
#'     fdnames = list('speed', 'sessions', 'time above threshold'))
#' dp.pca <- pca.fd(dpFun, nharm = 4)
#' ## 1st harmonic  captures vast majority of the variation
#' dp.pca$varprop
#' ## time spent above speed = 0 is the characteristic distinguishing the profiles
#' plot(dp.pca, harm = 1)
#' sumRuns <- summary(runs)
#' plot(sumRuns$durationMoving, dp.pca$scores[,1])
#' @export
profile2fd <- function(object, what, ...) {
    
    ## README: make it a method for the two classes?
    
    ## check/process input
    if (length(what) > 1) {
        what <- what[1]
        warning("Only the first element of 'what' is used.")
    }
    if (!(what %in% names(object))) 
        stop(paste("Profiles for", what, "not found."))
    
    ## get relevant data
    profiles <- object[[what]]
    grid <- zoo::index(profiles)
    
    ## prepare matrix
    profilesMat <- matrix(unlist(profiles), nrow = length(grid), dimnames = list(grid, 
        names(profiles)))
    
    ## remove sessions for which the whole profiles consists of NA only
    na <- apply(profilesMat, 2, function(x) all(is.na(x)))
    profilesMat <- profilesMat[, !na, drop = FALSE]
    
    ## contruct functional data object
    fd <- fda::Data2fd(argvals = grid, y = profilesMat, ...)
    
    return(fd)
}



#' Functional principal components analysis of distribution or concentration profiles.
#'
#' @param what The variable for which the profiles should be analysed.
#' @param nharm The number of principal components estimated.
#' @details The \code{...} argument is passed on to \code{\link[fda]{pca.fd}}.
#' @return An object of class \code{trackeRfpca}.
#' @references Ramsay JO, Silverman BW (2005). Functional Data Analysis. Springer-Verlag New York.
#' @examples
#' data('runs', package = 'trackeR')
#' dp <- distributionProfile(runs, what = 'speed')
#' dp.pca <- funPCA(dp, what = 'speed', nharm = 4)
#' ## 1st harmonic  captures vast majority of the variation
#' plot(dp.pca, harm = 1)
#' ## time spent above speed = 0 is the characteristic distinguishing the profiles
#' sumRuns <- summary(runs)
#' plot(sumRuns$durationMoving, dp.pca$scores[,1])
#' @name funPCA
#' @export
funPCA.distrProfile <- function(object, what, nharm = 4, ...) {
    
    ## transform to functional data format
    fdname3 <- if (class(object) == "distrProfile") 
        "Time above threshold" else "d time"
    fd <- profile2fd(object, what = what, fdnames = c(what, "Session", fdname3))
    
    ## fit functional PCA
    fpca <- fda::pca.fd(fd, nharm = nharm, ...)
    
    ## class and return
    attr(fpca, "profile") <- class(object)
    attr(fpca, "grid") <- zoo::index(object[[what]])
    attr(fpca, "what") <- what
    attr(fpca, "units") <- getUnits(object)
    class(fpca) <- c("trackeRfpca", class(fpca))
    return(fpca)
}

## function works for both types of profiles
#' @rdname funPCA
#' @export
funPCA.conProfile <- funPCA.distrProfile



#' Plot function for functional principal components analysis of distribution and concentration profiles.
#' 
#' @param x An object of class \code{trackeRfpca} as returned by \code{\link{funPCA}}.
#' @param harm A numerical vector of the harmonics to be plotted. Defaults to all harmonics.
#' @param expand The factor used to generate suitable multiples of the harmonics.
#'     If \code{NULL}, the effect of +/- 2 standard deviations of each harmonic is plotted.
#' @param pointplot Should the harmonics be plotted with \code{+} and \code{-}
#'     point characters? Otherwise, lines are used.
#' @param ... Currently not used.
#' @seealso \code{\link[fda]{plot.pca.fd}}
#' @references Ramsay JO, Silverman BW (2005). Functional Data Analysis. Springer-Verlag New York.
#' @examples
#' data('runs', package = 'trackeR')
#' dp <- distributionProfile(runs, what = 'speed')
#' dp.pca <- funPCA(dp, what = 'speed', nharm = 4)
#' ## 1st harmonic  captures vast majority of the variation
#' plot(dp.pca)
#' plot(dp.pca, harm = 1, pointplot = FALSE)
#' @export
plot.trackeRfpca <- function(x, harm = NULL, expand = NULL, pointplot = TRUE, ...) {
    ## function(x, nx = 128, pointplot = TRUE, harm = 0, expand = 0, cycle = FALSE, ...)
    ## argvals <- seq(x[['harmonics']]$basis$rangeval[1],
    ## x[['harmonics']]$basis$rangeval[2], length = 128) harm <- NULL expand <- NULL
    
    ## get grid over which the profiles and harmonics are plotted
    argvals <- attr(x, "grid")
    
    ## get mean function (evaluated at grid)
    meanMat <- fda::eval.fd(argvals, x[["meanfd"]])
    dimnames(meanMat) <- NULL
    
    ## get harmonics (evaluated at grid)
    harmonicsMat <- fda::eval.fd(argvals, x[["harmonics"]])
    if (is.null(harm)) 
        harm <- seq_len(length(x[["varprop"]]))
    nharm <- length(harm)
    
    ## combine to data.frame in long format for plotting
    df <- data.frame(argval = rep(argvals, times = nharm), meanval = rep(meanMat, times = nharm), 
        pc = rep(harm, each = length(argvals)), pcval = as.vector(harmonicsMat[, harm]))
    
    ## add suitable multitude of the harmonics to the mean function
    if (is.null(expand)) 
        expand <- sqrt(x[["values"]][harm])
    fac <- expand[rep(seq_along(harm), each = length(argvals))]
    df[, "meanPlusPC"] <- df[, "meanval"] + fac * df[, "pcval"]
    df[, "meanMinusPC"] <- df[, "meanval"] - fac * df[, "pcval"]
    
    ## label function for x axis (variable [unit])
    units <- getUnits(x)
    lab_data <- function(xx) {
        thisunit <- units$unit[units$variable == xx]
        prettyUnit <- prettifyUnits(thisunit)
        paste0(xx, " [", prettyUnit, "]")
    }
    # lab_data <- Vectorize(lab_data)
    
    ## base layer
    p <- ggplot2::ggplot(data = df) + ggplot2::labs(x = lab_data(attr(x, "what")), y = if (attr(x, 
        "profile") == "distrProfile") 
        "time above threshold" else "d time")
    
    ## mean function
    p <- p + ggplot2::geom_line(ggplot2::aes_(x = quote(argval), y = quote(meanval)))
    
    ## multiples of harmonics added/subtracted
    if (pointplot) {
        p <- p + ggplot2::geom_point(ggplot2::aes_(x = quote(argval), y = quote(meanPlusPC)), 
            pch = "+") + ggplot2::geom_point(ggplot2::aes_(x = quote(argval), y = quote(meanMinusPC)), 
            pch = "-")
    } else {
        p <- p + ggplot2::geom_line(ggplot2::aes_(x = quote(argval), y = quote(meanPlusPC)), 
            lty = 2) + ggplot2::geom_line(ggplot2::aes_(x = quote(argval), y = quote(meanMinusPC)), 
            lty = 3)
    }
    
    ## facets for harmonics create lookup table
    lab_pc <- function(pc) {
        ret <- paste0("PC ", pc, " (", round(x[["varprop"]][pc] * 100, 1), "% of variability)")
        names(ret) <- pc
        ret
    }
    lab_pc <- lab_pc(harm)
    
    mfrow <- grDevices::n2mfrow(length(harm))
    p <- p + ggplot2::facet_wrap("pc", ncol = mfrow[2], labeller = ggplot2::as_labeller(lab_pc))
    
    p <- p + ggplot2::theme_bw()
    return(p)
    
}
