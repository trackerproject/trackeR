## FIXME: need data to show selection of sessions and variables...

#' Plot training sessions in form of trackeRdata objects.
#'
#' @param x An object of class \code{\link{trackeRdata}}.
#' @param session A numeric vector of the sessions to be plotted, defaults to all sessions.
#' @param what Which variables should be plotted?
#' @param threshold Logical. Should thresholds be applied?
#' @param smooth Logical. Should the data be smoothed?
#' @param trend Logical. Should a smooth trend be plotted?
#' @param dates Logical. Should the date of the session be used in the panel header?
#' @param ... Further arguments to be passed to \code{\link{threshold.trackeRdata}} and
#'     \code{\link{smootherControl.trackeRdata}}.
#' @examples
#' data(run, package = "trackeR")
#' plot(run)
#' ## plot raw data
#' plot(run, threshold = FALSE, smooth = FALSE)
#' ## threshold speed variable
#' plot(run, threshold = TRUE, smooth = FALSE, variable = "speed", lower = 0, upper = 10)
#' ## and smooth (thresholding with default values)
#' plot(run, threshold = TRUE, smooth = TRUE, width = 30)
#' ## change units
#' plot(changeUnits(run, variable = "speed", unit = "km_per_h"))
#' @export
plot.trackeRdata <- function(x, session = NULL, what = c("speed", "heart.rate"),
                             threshold = TRUE, smooth = FALSE, trend = TRUE, dates = TRUE, ...){
    ## code inspired by autoplot.zoo
    if (is.null(session)) session <- seq_along(x)
    units <- getUnits(x)

    ## threshold
    if (threshold){
        dots <- list(...)
        if (all(c("variable", "lower", "upper") %in% names(dots))){
            ## thresholds provided by user
            th <- data.frame(variable = dots$variable, lower = dots$lower, upper = dots$upper)
        } else {
            ## default thresholds
            cycling <- units$unit[units$variable == "cadence"] == "rev_per_min"
            th <- generateDefaultThresholds(cycling)
            th <- changeUnits(th, variable = units$variable, unit = units$unit)
        }
        ## apply thresholds
        x <- threshold(x, th)
    }

    ## smooth
    if (smooth) {
        if (is.null(getOperations(x)$smooth)) {
            x <- smoother(x, what = what, session = session, ...)
        } else {
            warning("This object has already been smoothed. No additional smoothing takes place.")
            x <- x[session]
        }
    } else {
        x <- x[session]
    }

    ## get data
    df <- fortify(x, melt = TRUE)

    ## prepare session id for panel header 
    if (dates) {
        df$SessionID <- format(session[df$SessionID])
        df$SessionID <- gsub(" ", "0", df$SessionID)
        df$SessionID <- paste(df$SessionID, format(df$Index, "%Y-%m-%d"), sep = ": ")
    }
    else {
        df$SessionID <- factor(df$SessionID, levels = seq_along(session), labels = session)
    }
    df <- subset(df, Series %in% what)
    df$Series <- factor(df$Series)

    ## check that there is data to plot
    for(l in levels(df$Series)){
        if (all(is.na(subset(df, Series == l, select = "Value"))))
            df <- df[!(df$Series == l), ]
    }

    ## make facets
    singleVariable <- nlevels(df$Series) == 1L
    singleSession <- length(session) == 1L
    facets <- if (singleVariable) {
        if (singleSession) NULL else . ~ SessionID
    } else {
        if(singleSession) Series ~ . else Series ~ SessionID
    }
    lab <- function(variable, value){
        if (variable == "Series"){
            ret <- paste0(value, " [", units$unit[units$variable == value], "]")
        } else {
            ret <- as.character(value)
        }
        return(ret)
    }
    lab <- Vectorize(lab)

    ## basic plot (make geom flexible?)
    p <- ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = Index, y = Value)) + ggplot2::geom_line() +
        ggplot2::ylab(if(singleVariable) lab("Series", levels(df$Series)) else "") + ggplot2::xlab("time")
    if (trend) p <- p + ggplot2::geom_smooth(method = "gam",
                                             formula = y ~ s(x, bs = "cs"),
                                             alpha = 0.5,
                                             se = FALSE)
    ## add facet if necessary
    if (!is.null(facets)){
        p <- p + ggplot2::facet_grid(facets, scales = "free", labeller = lab)
    }
    ## add bw theme
    p <- p + ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 50, hjust = 1))

    return(p)
}


#' Fortify a trackeRdata object for plotting with ggplot2.
#'
#' @param model The \code{\link{trackeRdata}} object.
#' @param data Ignored.
#' @param melt Logical. Should the data be melted into long format
#'     instead of the default wide format?
#' @param ... Ignored.
#' @export
fortify.trackeRdata <- function(model, data, melt = FALSE, ...){
    ret <- list()
    for (i in seq_along(model)){

        ret[[i]] <- zoo::fortify.zoo(model[[i]], melt = melt)
        ret[[i]]$SessionID <- i
        ## FIXME: add date identifier?
    }
    ret <- do.call("rbind", ret)
    return(ret)
}


#' Plot routes for training sessions.
#'
#' Plot the route ran/cycled during training onto a background map.
#' Internet connection is required to download the background map.
#'
#' @param x A object of class \code{\link{trackeRdata}}.
#' @param session The session to be plotted.
#' @param zoom The zoom level for the background map as passed on to
#'     \code{\link[ggmap]{get_map}} (2 corresponds roughly to continent
#'     level and 20 to building level).
#' @param speed Logical. Should the trace be colored according to speed?
#' @param ... Additionall arguments passed on to \code{\link[ggmap]{get_map}}, e.g.,
#'     \code{source} and \code{maptype}.
#' @seealso \code{\link[ggmap]{get_map}}, \code{\link[ggmap]{ggmap}}
#' @examples
#' \dontrun{
#' data(run, package = "trackeR")
#' plotRoute(run, zoom = 13)
#' plotRoute(run, zoom = 13, maptype = "hybrid")
#' plotRoute(run, zoom = 13, source = "osm")
#' }
#' @export
plotRoute <- function(x, session = 1, zoom = NULL, speed = TRUE, ...){

    ## get data
    x <- x[[session]]
    df <- fortify(x, melt = FALSE)

    ## clean data
    df <- df[, c("longitude", "latitude", "speed")]
    df <- df[!apply(df[, c("longitude", "latitude")], 1, function(x) any(is.na(x))), ]

    ## get range of coordinates
    rangeLon <- range(df$longitude, na.rm = TRUE)
    rangeLat <- range(df$latitude, na.rm = TRUE)

    ## convert range to center and zoom (adapted from ggmap::get_map)
    lengthLon <- diff(rangeLon)
    lengthLat <- diff(rangeLat)
    centerLon <- rangeLon[1] + lengthLon / 2
    centerLat <- rangeLat[1] + lengthLat / 2
    if (is.null(zoom)) {
        zoomLon <- ceiling(0.9*log2(360 * 2 / lengthLon))
        zoomLat <- ceiling(0.9*log2(180 * 2 / lengthLat))
        zoom <- max(zoomLon, zoomLat)
    }

    ## get map
    map <- ggmap::get_map(location = c(lon = centerLon, lat = centerLat), zoom = zoom, ...)
    map <- ggmap::ggmap(map)

    ## prepare df for geom_segments
    df$longitude0 <- c(df$longitude[-nrow(df)], 0)
    df$longitude1 <- c(df$longitude[-1], 0)
    df$latitude0 <- c(df$latitude[-nrow(df)], 0)
    df$latitude1 <- c(df$latitude[-1], 0)
    df <- df[-nrow(df), ]

    ## add trace
    if (speed){
        p <- map + ggplot2::geom_segment(ggplot2::aes(x = longitude0, xend = longitude1,
                                                 y = latitude0, yend = latitude1,
                                                 color = speed), data = df, lwd = 1, alpha = 0.8) +
                                                     ggplot2::labs(x = "longitude", y = "latitude")
    } else {
        p <- map + ggplot2::geom_segment(ggplot2::aes(x = longitude0, xend = longitude1,
                                                 y = latitude0, yend = latitude1),
                                    data = df, lwd = 1, alpha = 0.8) +
                                                     ggplot2::labs(x = "longitude", y = "latitude")
    }

    return(p)

}
