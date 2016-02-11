#' Plot training sessions in form of trackeRdata objects.
#'
#' @param x An object of class \code{\link{trackeRdata}}.
#' @param session A numeric vector of the sessions to be plotted, defaults to all sessions.
#' @param what Which variables should be plotted?
#' @param threshold Logical. Should thresholds be applied?
#' @param smooth Logical. Should the data be smoothed?
#' @param trend Logical. Should a smooth trend be plotted?
#' @param dates Logical. Should the date of the session be used in the panel header?
#' @param ... Further arguments to be passed to \code{\link{threshold}} and
#'     \code{\link{smootherControl.trackeRdata}}.
#' @details Note that a threshold is always applied to the pace. This (upper) threshold
#'     corresponds to a speed of 1.4 meters per second, the preferred walking speed of
#'     humans. The lower threshold is 0.
#' @examples
#' \dontrun{
#' data(runs, package = "trackeR")
#' ## plot heart rate and pace for the first 3 sessions
#' plot(runs, session = 1:3)
#' ## plot raw speed data for session 4
#' plot(runs, session = 4, what = "speed", threshold = FALSE, smooth = FALSE)
#' ## threshold speed variable
#' plot(runs, session = 4, what = "speed", threshold = TRUE, smooth = FALSE,
#'     variable = "speed", lower = 0, upper = 10)
#' ## and smooth (thresholding with default values)
#' plot(runs, session = 4, what = "speed", threshold = TRUE,
#'     smooth = TRUE, width = 15, parallel = FALSE)
#' }
#' @export
plot.trackeRdata <- function(x, session = NULL, what = c("pace", "heart.rate"),
                             threshold = TRUE, smooth = FALSE, trend = TRUE, dates = TRUE, ...){
    ## the following line is just intended to prevent R CMD check to produce the NOTE
    ## "no visible binding for global variable 'Series'" because that variable is used in subset()
    Series <- NULL

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
            ## th <- th[which(th$variable %in% what),]
            ## w <- which(units$variable %in% what)
            th <- changeUnits(th, variable = units$variable, unit = units$unit)
        }
        ## apply thresholds
        x <- threshold(x, th)
    }

    ## for plotting pace, always apply a threshold
    ## upper threshold is based on preferred walking speed of 1.4 m/s,
    ## see https://en.wikipedia.org/wiki/Preferred_walking_speed
    if ("pace" %in% what){
        conversionPace <- match.fun(paste("s_per_m", units$unit[units$variable == "pace"], sep = "2"))
        thPace <- conversionPace(1 / 1.4)
        x <- threshold(x, variable = "pace", lower = 0, upper = thPace)
    }

    ## smooth
    if (smooth) {
        xo <- x[session]
        if (is.null(getOperations(x)$smooth)) {
            x <- smoother(x, what = what, session = session, ...)
        } else {
            warning("This object has already been smoothed. No additional smoothing takes place.")
            smooth <- FALSE ## it's not the plot function calling smoother
            x <- x[session]
        }
    } else {
        x <- x[session]
    }

    ## get data
    df <- if (smooth) fortify(xo, melt = TRUE) else fortify(x, melt = TRUE)

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
        if (singleSession) NULL else ". ~ SessionID"
    } else {
        if(singleSession) "Series ~ ." else "Series ~ SessionID"
    }
    ## lab <- function(variable, value){
    ##     if (variable == "Series"){
    ##         ret <- paste0(value, " [", units$unit[units$variable == value], "]")
    ##     } else {
    ##         ret <- as.character(value)
    ##     }
    ##     return(ret)
    ## }
    ## lab <- Vectorize(lab)
    ## new (todo: make units an argument and move outside of plotting function
    lab_data <- function(series){
        thisunit <- units$unit[units$variable == series]
        prettyUnit <- prettifyUnits(thisunit)
        paste0(series, " [", prettyUnit,"]")
    }
    lab_data <- Vectorize(lab_data)

    ## basic plot 
    p <- ggplot2::ggplot(data = df, mapping = ggplot2::aes_(x = quote(Index), y = quote(Value))) +
        ggplot2::geom_line(color = if (smooth) "gray" else "black") +
        ggplot2::ylab(if(singleVariable) lab_data(levels(df$Series)) else "") + ggplot2::xlab("Time")
    if (trend & !smooth){
        p <- p + ggplot2::geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), alpha = 0.5, se = FALSE)
    }
    ## add facet if necessary
    if (!is.null(facets)){
        p <- p + ggplot2::facet_grid(facets, scales = "free", labeller = ggplot2::labeller("Series" = lab_data))
    }
    ## add bw theme
    p <- p + ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 50, hjust = 1))


    ## if plot did smoothing add smoothed data on top of plot
    if (smooth){
        ## data prep
        dfs <- fortify(x, melt = TRUE)
        if (dates) {
            dfs$SessionID <- format(session[dfs$SessionID])
            dfs$SessionID <- gsub(" ", "0", dfs$SessionID)
            dfs$SessionID <- paste(dfs$SessionID, format(dfs$Index, "%Y-%m-%d"), sep = ": ")
        }
        else {
            dfs$SessionID <- factor(dfs$SessionID, levels = seq_along(session), labels = session)
        }
        dfs <- subset(dfs, Series %in% what)
        dfs$Series <- factor(dfs$Series)
        for(l in levels(dfs$Series)){
            if (all(is.na(subset(dfs, Series == l, select = "Value"))))
                dfs <- dfs[!(dfs$Series == l), ]
        }
        ## add plot layers
        p <- p + ggplot2::geom_line(ggplot2::aes_(x = quote(Index), y = quote(Value)), data = dfs, col = "black")
        if (trend){
            p <- p + ggplot2::geom_smooth(data = dfs, method = "gam", formula = y ~ s(x, bs = "cs"),
                                          alpha = 0.5, se = FALSE)
        }
    }

    return(p)
}

prettifyUnit <- function(unit){
    unit <- as.character(unit)
    prettyUnit <- switch(unit,
                         m_per_s = "m/s",
                         km_per_h = "km/h",
                         ft_per_min = "ft/min",
                         ft_per_s = "ft/s",
                         mi_per_h = "mi/h",
                         steps_per_min = "steps/min",
                         rev_per_min = "revolutions/min",
                         min_per_km = "min/km",
                         min_per_mi = "min/mi",
                         s_per_m = "s/m",
                         as.character(unit))
    return(prettyUnit)
}
prettifyUnits <- Vectorize(prettifyUnit)


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
#' @param threshold Logical. Should thresholds be applied?
#' @param ... Additional arguments passed on to \code{\link{threshold}} and
#'     \code{\link[ggmap]{get_map}}, e.g., \code{source} and \code{maptype}.
#' @seealso \code{\link[ggmap]{get_map}}, \code{\link[ggmap]{ggmap}}
#' @examples
#' \dontrun{
#' data(runs, package = "trackeR")
#' plotRoute(runs, session = 4, zoom = 13)
#' plotRoute(runs, session = 4, zoom = 13, maptype = "hybrid")
#' plotRoute(runs, session = 4, zoom = 13, source = "osm")
#' }
#' @export
plotRoute <- function(x, session = 1, zoom = NULL, speed = TRUE, threshold = TRUE, ...){
    
    units <- getUnits(x)

    ## get sessions 
    x <- x[session]

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
    ## get data
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
        p <- map + ggplot2::geom_segment(
                       ggplot2::aes_(x = quote(longitude0), xend = quote(longitude1),
                                     y = quote(latitude0), yend = quote(latitude1),
                                    color = quote(speed)),
                       data = df, lwd = 1, alpha = 0.8) +
            ggplot2::labs(x = "Longitude", y = "Latitude") +
            ggplot2::guides(color = ggplot2::guide_colorbar(title = "Speed"))
    } else {
        p <- map + ggplot2::geom_segment(
                       ggplot2::aes_(x = quote(longitude0), xend = quote(longitude1),
                                     y = quote(latitude0), yend = quote(latitude1)),
                       data = df, lwd = 1, alpha = 0.8) +
            ggplot2::labs(x = "Longitude", y = "Latitude")
    }

    return(p)
}
