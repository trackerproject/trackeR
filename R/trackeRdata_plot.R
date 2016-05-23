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
#' data("runs", package = "trackeR")
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
        xo <- x
        if (is.null(getOperations(x)$smooth)) {
            x <- smoother(x, what = what, ...)
        } else {
            warning("This object has already been smoothed. No additional smoothing takes place.")
            smooth <- FALSE ## it's not the plot function calling smoother
            x <- x
        }
    } else {
        x <- x
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
    ## Include the labels even if there is a single session. This will
    ## have the (undesirable?) effect that sessions which extend
    ## beyond midnight will be split in two panels at midnight
    singleSession <- FALSE #length(session) == 1L

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
        ggplot2::geom_line(color = if (smooth) "gray" else "black", na.rm = TRUE) +
        ggplot2::ylab(if(singleVariable) lab_data(levels(df$Series)) else "") + ggplot2::xlab("Time")
    if (trend & !smooth){
        p <- p + ggplot2::geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),
                                      alpha = 0.5, se = FALSE, na.rm = TRUE)
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
        p <- p + ggplot2::geom_line(ggplot2::aes_(x = quote(Index), y = quote(Value)),
                                    data = dfs, col = "black", na.rm = TRUE)
        if (trend){
            p <- p + ggplot2::geom_smooth(data = dfs, method = "gam", formula = y ~ s(x, bs = "cs"),
                                          alpha = 0.5, se = FALSE, na.rm = TRUE)
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
#' @param session A numeric vector of the sessions to be plotted. Defaults
#'     to the first session, all sessions can be plotted by \code{session = NULL}.
#' @param zoom The zoom level for the background map as passed on to
#'     \code{\link[ggmap]{get_map}} (2 corresponds roughly to continent
#'     level and 20 to building level).
#' @param speed Logical. Should the trace be colored according to speed?
#' @param threshold Logical. Should thresholds be applied?
#' @param mfrow A vector of 2 elements, number of rows and number of columns,
#'     specifying the layout for multiple sessions.
#' @param ... Additional arguments passed on to \code{\link{threshold}} and
#'     \code{\link[ggmap]{get_map}}, e.g., \code{source} and \code{maptype}.
#' @seealso \code{\link[ggmap]{get_map}}, \code{\link[ggmap]{ggmap}}
#' @examples
#' \dontrun{
#' data("runs", package = "trackeR")
#' plotRoute(runs, session = 4, zoom = 13)
#' plotRoute(runs, session = 4, zoom = 13, maptype = "hybrid")
#' plotRoute(runs, session = 4, zoom = 13, source = "osm")
#' ## multiple sessions
#' plotRoute(runs, session = c(1:5, 8:11), zoom = 10, source = "osm")
#' ## different zoom level per panel
#' plotRoute(runs, session = 6:7, source = "osm", zoom = c(13, 14))
#' }
#' @export
plotRoute <- function(x, session = 1, zoom = NULL, speed = TRUE, threshold = TRUE, mfrow = NULL, ...){

    ## prep
    if (is.null(session)) session <- seq_along(x)
    if (!is.null(zoom)) zoom <- rep(zoom, length.out = length(session))


    ## get prepared data.frame
    df <- prepRoute(x, session = session, threshold = threshold, ...)
    centers <- attr(df, "centers")

    if (speed) speedRange <- range(df[["speed"]], na.rm = TRUE)

    ## loop over sessions
    plotList <- vector("list", length(session))
    names(plotList) <- as.character(session)

    for (ses in session){

        dfs <- df[df$SessionID == which(ses == session), , drop = FALSE]
        zooms <- if (is.null(zoom)) centers[centers$SessionID == ses, "zoom"] else zoom[which(ses == session)]

        ## get map
        map <- ggmap::get_map(location = c(lon = centers[centers$SessionID == ses, "centerLon"],
                                           lat = centers[centers$SessionID == ses, "centerLat"]),
                              zoom = zooms, ...)
        p <- ggmap::ggmap(map)

        ## add trace
        if (speed){
            p <- p + ggplot2::geom_segment(
                         ggplot2::aes_(x = quote(longitude0), xend = quote(longitude1),
                                       y = quote(latitude0), yend = quote(latitude1),
                                       color = quote(speed)),
                         data = dfs, lwd = 1, alpha = 0.8, na.rm = TRUE) +
                ##ggplot2::guides(color = ggplot2::guide_colorbar(title = "Speed"))
                ggplot2::scale_color_gradient(limits = speedRange,
                                              guide = ggplot2::guide_colorbar(title = "Speed"))
        } else {
            p <- p + ggplot2::geom_segment(
                         ggplot2::aes_(x = quote(longitude0), xend = quote(longitude1),
                                       y = quote(latitude0), yend = quote(latitude1)),
                         data = dfs, lwd = 1, alpha = 0.8, na.rm = TRUE)
        }


        ## Extract legend from the first plot
        if (ses == session[1] & speed) {
            legend <- gtable::gtable_filter(ggplot2::ggplot_gtable(ggplot2::ggplot_build(p)), "guide-box")
        }

        p <- p + ggplot2::labs(title = paste("Session:", ses))
                               ## x = "Longitude", y = "Latitude")
        plotList[[as.character(ses)]] <- p +  ggplot2::theme(legend.position = "none",
                                                             axis.title.x = ggplot2::element_blank(),
                                                             axis.title.y = ggplot2::element_blank())
    }

    ## arrange separate plots
    if (is.null(mfrow))  mfrow <- grDevices::n2mfrow(length(session))
    arrange <- function(...) gridExtra::arrangeGrob(..., nrow = mfrow[1], ncol = mfrow[2],
                                                     left = grid::textGrob("Latitude", rot = 90),
                                                     bottom = grid::textGrob("Longitude", rot = 00))

    if (speed)
        gridExtra::grid.arrange(do.call(arrange, plotList),
                                legend = if (speed) legend else NULL,
                                widths = grid::unit.c(grid::unit(1, "npc") - legend$width, legend$width), nrow = 1)
    else
        gridExtra::grid.arrange(do.call(arrange, plotList))
}


#' Plot routes for training sessions.
#'
#' Plot the route ran/cycled during training on an interactive map.
#' Internet connection is required to download the background map.
#' Icons are by Maps Icons Collection \url{https://mapicons.mapsmarker.com}
#'
#' @param x A object of class \code{\link{trackeRdata}}.
#' @param session A numeric vector of the sessions to be plotted. Defaults to
#'     all sessions.
#' @param threshold Logical. Should thresholds be applied?
#' @param ... Additional arguments passed on to \code{\link{threshold}}.
#' @examples
#' \dontrun{
#' data("runs", package = "trackeR")
#' leafletRoute(runs, session = 22:23)
#' }
#' @export
leafletRoute <- function(x, session = NULL, threshold = TRUE, ...){

    if (is.null(session)) session <- seq_along(x)

    ## get prepared data.frame
    df <- prepRoute(x, session = session, threshold = threshold, ...)

    ## prepare markers
    startIcon <- leaflet::makeIcon(
        iconUrl = system.file("icons", "start.png", package = "trackeR"),
        iconWidth = 32, iconHeight = 37, iconAnchorX = 16, iconAnchorY = 37
    )
    finishIcon <- leaflet::makeIcon(
        iconUrl = system.file("icons", "finish.png", package = "trackeR"),
        iconWidth = 32, iconHeight = 37, iconAnchorX = 16, iconAnchorY = 37
    )

    ## prepare popups
    units <- getUnits(x)
    sumX <- summary(x)
    popupText <- function(session, start = TRUE){
        w <- which(sumX$session == session)
        unitDist4pace <- strsplit(units$unit[units$variable == "pace"],
                                  split = "_per_")[[1]][2]
        avgPace <- floor(sumX$avgPace[w] * 100) / 100

        paste(
            paste("<b>", ifelse(start, "Start", "End"), "of session", session, "</b>"),
            paste(sumX$sessionStart[w], "-", sumX$sessionEnd[w]),
            paste("Distance:", round(sumX$distance[w], 2), units$unit[units$variable == "distance"]),
            paste("Duration:", round(as.numeric(sumX$duration[w]), 2), units(sumX$duration[w])),
            paste(paste0("Avg. pace (per 1 ", unitDist4pace, "):"),
                  paste(floor(avgPace), round(avgPace %% 1 * 60, 0), sep = ":"), "min:sec"),
            sep = "<br/>"
        )
    }

    ## get map
    p <- leaflet::leaflet()
    p <- leaflet::addTiles(p, group = "OSM (default)")
    p <- leaflet::addProviderTiles(p, "Stamen.Toner", group = "Toner")
    p <- leaflet::addProviderTiles(p, "Stamen.TonerLite", group = "Toner Lite")

    ## add trace + markers + popups
    for (i in session){
        dfi <- df[df$SessionID == which(i == session), , drop = FALSE]
        p <- leaflet::addPolylines(p, group = paste("Session:", i),
                                   lng = dfi$longitude, lat = dfi$latitude)

        p <- leaflet::addMarkers(p, group = paste("Session:", i),
                                 lng = dfi$longitude[1], lat = dfi$latitude[1],
                                 #popup = htmltools::htmlEscape(popupText(session = i)))
                                 popup = popupText(session = i, start = TRUE), icon = startIcon)
        p <- leaflet::addMarkers(p, group = paste("Session:", i),
                                 lng = dfi$longitude[nrow(dfi)], lat = dfi$latitude[nrow(dfi)],
                                 popup = popupText(session = i, start = FALSE), icon = finishIcon)
    }

    ## color trace according to speed is disabled for now as it is
    ## typically too slow to plot all the segments separately.
    ##
    ## if (speed){
    ##     ncol <- 10
    ##     mypal <- colorspace::heat_hcl(n = ncol)

    ##     for (i in session){
    ##         dfi <- df[df$SessionID == which(i == session), , drop = FALSE]

    ##         speedCat <- cut(df$speed, breaks = seq(min(df$speed), max(df$speed), length.out = ncol + 1),
    ##                         include.lowest = TRUE, labels = FALSE)
    ##         mycol <- mypal[speedCat]
    ##         for (j in 1:nrow(dfi)){
    ##             p <- leaflet::addPolylines(p, group = paste("Session:", i),
    ##                                        lng = c(dfi$longitude0[j], dfi$longitude1[j]),
    ##                                        lat = c(dfi$latitude0[j], dfi$latitude1[j]),
    ##                                        col = mycol[j])
    ##         }
    ##         ## ## alternative for making the palette
    ##         ## pal <- leaflet::colorNumeric(palette = mypal, domain = dfi$speed)
    ##         ## ## however, still just a single colour for the entire path
    ##         ## p <- leaflet::addPolylines(p, group = paste("Session:", i),
    ##         ##                            lng = dfi$longitude, lat = dfi$latitude,
    ##         ##                            color = pal(dfi$speed))
    ##         p <- leaflet::addMarkers(p, group = paste("Session:", i),
    ##                         lng = dfi$longitude[1], lat = dfi$latitude[1],
    ##                         popup = htmltools::htmlEscape(paste("Start session", i)))
    ##         p <- leaflet::addMarkers(p, group = paste("Session:", i),
    ##                         lng = dfi$longitude[nrow(dfi)], lat = dfi$latitude[nrow(dfi)],
    ##                         popup = htmltools::htmlEscape(paste("End session", i)))
    ##     }
    ## }

    ## add control panel
    p <- leaflet::addLayersControl(p, baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
                          overlayGroups = paste("Session:", session),
                          options = leaflet::layersControlOptions(collapsed = FALSE))

    return(p)
}


prepRoute <- function(x, session = 1, threshold = TRUE, ...){
    ## get units for thresholds
    units <- getUnits(x)

    ## get sessions
    if (is.null(session)) session <- seq_along(x)
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
    if (length(session) < 2) df$SessionID <- 1

    ## clean data
    df <- df[, c("longitude", "latitude", "speed", "SessionID")]
    df <- df[!apply(df[, c("longitude", "latitude")], 1, function(x) any(is.na(x))), ]

    ## get range of coordinates for all sessions
    rangeLon <- range(df$longitude, na.rm = TRUE)
    rangeLat <- range(df$latitude, na.rm = TRUE)

    ## convert range to center and zoom (adapted from ggmap::get_map)
    lengthLon <- diff(rangeLon)
    lengthLat <- diff(rangeLat)
    centerLon <- rangeLon[1] + lengthLon / 2
    centerLat <- rangeLat[1] + lengthLat / 2
    zoomLon <- ceiling(0.9*log2(360 * 2 / lengthLon))
    zoomLat <- ceiling(0.9*log2(180 * 2 / lengthLat))
    zoom <- max(zoomLon, zoomLat)


    dfSplit <- centers <- vector("list", length(session))
    names(dfSplit) <- names(centers) <- as.character(session)

    ## centers <- data.frame(session, NA, NA, NA)
    ## names(centers) <- c("SessionID", "centerLon", "centerLat", "zoom")

    for (i in session){
        ## get subset for session
        dfSub <- df[df$SessionID == which(i == session), , drop = FALSE]

        ## get range of coordinates
        rangeLonI <- range(dfSub$longitude, na.rm = TRUE)
        rangeLatI <- range(dfSub$latitude, na.rm = TRUE)

        ## convert range to center and zoom (adapted from ggmap::get_map)
        lengthLonI <- diff(rangeLonI)
        lengthLatI <- diff(rangeLatI)
        centerLonI <- rangeLonI[1] + lengthLonI / 2
        centerLatI <- rangeLatI[1] + lengthLatI / 2
        zoomLonI <- ceiling(0.9*log2(360 * 2 / lengthLonI))
        zoomLatI <- ceiling(0.9*log2(180 * 2 / lengthLatI))
        zoomI <- max(zoomLonI, zoomLatI)

        centers[[as.character(i)]] <- c(centerLonI, centerLatI, zoomI)

        ## prep lon/lat for segments
        dfSub$longitude0 <- c(dfSub$longitude[-nrow(dfSub)], 0)
        dfSub$longitude1 <- c(dfSub$longitude[-1], 0)
        dfSub$latitude0 <- c(dfSub$latitude[-nrow(dfSub)], 0)
        dfSub$latitude1 <- c(dfSub$latitude[-1], 0)

        dfSplit[[as.character(i)]] <- dfSub[-nrow(dfSub), ]
    }
    df <- do.call(rbind, dfSplit)
    centers <- data.frame(session, do.call(rbind, centers))
    names(centers) <- c("SessionID", "centerLon", "centerLat", "zoom")

    ## add attributes and return
    attr(df, "centerLon") <- centerLon
    attr(df, "centerLat") <- centerLat
    attr(df, "autozoom") <- zoom
    attr(df, "centers") <- centers
    return(df)
}


#' @export
timeline.trackeRdata <- function(object, lims = NULL, ...) {
    sobject <- summary.trackeRdata(object, ...)
    timeline.trackeRdataSummary(sobject, lims = lims)
}
