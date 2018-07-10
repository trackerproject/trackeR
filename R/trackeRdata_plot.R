#' Plot training sessions in form of trackeRdata objects
#'
#' @param x An object of class \code{\link{trackeRdata}}.
#' @param session A numeric vector of the sessions to be plotted,
#'     defaults to all sessions.
#' @param what Which variables should be plotted?
#' @param threshold Logical. Should thresholds be applied?
#' @param smooth Logical. Should the data be smoothed?
#' @param trend Logical. Should a smooth trend be plotted?
#' @param dates Logical. Should the date of the session be used in the
#'     panel header?
#' @param moving_threshold A named vector of 3 speeds to be used for
#'     thresholding pace, given in the unit of the speed measurements
#'     in \code{object}. If \code{NULL} (default), the speeds are
#'     taken to be \code{c(cycling = 2, running = 1, swimming =
#'     0.5)}. See Details.
#' @param unit_reference_sport The sport to inherit units from
#'     (default is taken to be the most frequent sport in
#'     \code{object}).
#' @param ... Further arguments to be passed to
#'     \code{\link{threshold}} and
#'     \code{\link{smootherControl.trackeRdata}}.
#' @details
#'
#' Note that a threshold is always applied to the pace. This (upper)
#' threshold corresponds to a speed of 1.4 meters per second, the
#' preferred walking speed of humans. The lower threshold is 0.
#'
#' The units for the variables match those of the sport specified by
#' \code{unit_reference_sport}.
#'
#' @examples
#' \dontrun{
#' data('runs', package = 'trackeR')
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
plot.trackeRdata <- function(x, session = NULL,
                             what = c("pace", "heart_rate"),
                             threshold = TRUE,
                             smooth = FALSE,
                             trend = TRUE,
                             dates = TRUE,
                             unit_reference_sport = NULL,
                             moving_threshold = NULL,
                             ...){
    units <- get_units(x)

    if (is.null(session)) {
        session <- seq_along(x)
    }

    if (is.null(unit_reference_sport)) {
        unit_reference_sport <- find_unit_reference_sport(x)
    }
    ## Match units to those of unit_reference_sport
    un <- collect_units(units, unit_reference_sport)
    for (va in unique(un$variable)) {
        units$unit[units$variable == va] <- un$unit[un$variable == va]
    }

    ## convert moving_threshold
    if (is.null(moving_threshold)) {
        moving_threshold <- c(cycling = 2, running = 1, swimming = 0.5)
        speed_unit <- un$unit[un$variable == "speed"]
        if (speed_unit != "m_per_s") {
            conversion <- match.fun(paste("m_per_s", speed_unit, sep = "2"))
            moving_threshold <- conversion(moving_threshold)
        }
    }


    x <- x[session]

    ## Change units to those of unit_reference_sport
    x <- changeUnits(x, units$variable, units$unit, units$sport)

    ## threshold
    if (threshold) {
        dots <- list(...)
        if (all(c("variable", "lower", "upper", "sport") %in% names(dots))) {
            th <- generate_thresholds(dots$variable, dots$lower, dots$upper, dots$sport)
        }
        else {
            ## default thresholds
            th <- generate_thresholds()
            th <- change_units(th, variable = units$variable, unit = units$unit, sport = units$sport)
        }
        ## apply thresholds
        x <- threshold(x, th$variable, th$lower, th$upper, th$sport)
    }

    ## for plotting pace, always apply a threshold
    ## upper threshold is based on moving thresholds
    ## see https://en.wikipedia.org/wiki/Preferred_walking_speed
    speed_unit <- strsplit(un$unit[un$variable == "speed"], split = "_per_")[[1]]
    pace_unit <- paste(speed_unit[2], speed_unit[1], sep = "_per_")
    convert_pace <- match.fun(paste(pace_unit, un$unit[un$variable == "pace"], sep = "2"))

    x <- threshold(x,
                   variable = c("pace", "pace", "pace"),
                   lower = c(0, 0, 0),
                   upper = convert_pace(1/moving_threshold),
                   sport = names(moving_threshold))

    ## smooth
    if (smooth) {
        xo <- x
        if (is.null(get_operations(x)$smooth)) {
            x <- smoother(x, what = what, ...)
        }
        else {
            warning("This object has already been smoothed. No additional smoothing takes place.")
            smooth <- FALSE ## it's not the plot function calling smoother
            x <- x
        }
    }
    else {
        x <- x
    }

    ## get data
    df <- if (smooth) fortify(xo, melt = TRUE) else fortify(x, melt = TRUE)

    ## prepare session id for panel header
    if (dates) {
        df$SessionID <- format(session[df$SessionID])
        df$SessionID <- gsub(" ", "0", df$SessionID)
        df$SessionID <- paste0(paste(df$SessionID, df$Sport, sep = ": "), "\n", format(df$Index, "%Y-%m-%d"))
    }
    else {
        df$SessionID <- paste0(paste(df$SessionID, df$Sport, sep = ": "))
    }
    df <- subset(df, Series %in% what)
    df$Series <- factor(df$Series)

    ## check that there is data to plot
    for (l in levels(df$Series)) {
        if (all(is.na(subset(df, Series == l, select = "Value"))))
            df <- df[!(df$Series == l), ]
    }

    facets <- "Series ~ SessionID"

    lab_data <- function(series) {
        thisunit <- un$unit[un$variable == series]
        prettyUnit <- prettifyUnits(thisunit)
        paste0(series, "\n[", prettyUnit,"]")
    }
    lab_data <- Vectorize(lab_data)

    ## basic plot
    p <- ggplot(data = df, mapping = aes_(x = quote(Index), y = quote(Value))) +
        geom_line(color = grDevices::gray(0.9), na.rm = TRUE) +
        ylab("") +
        xlab("Time")

    if (trend & !smooth){
        p <- p + geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),
                                      se = FALSE, na.rm = TRUE, lwd = 0.5, col = "black")
    }

    ## add facet if necessary
    if (!is.null(facets)){
        p <- p + facet_grid(facets, scales = "free", labeller = labeller("Series" = lab_data))
    }

    ## add bw theme
    p <- p +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 50, hjust = 1),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank())


    ## if plot did smoothing add smoothed data on top of plot
    if (smooth){
        ## data prep
        dfs <- fortify(x, melt = TRUE)

        if (dates) {
            dfs$SessionID <- format(session[dfs$SessionID])
            dfs$SessionID <- gsub(" ", "0", dfs$SessionID)
            dfs$SessionID <- paste0(paste(dfs$SessionID, dfs$Sport, sep = ": "), "\n", format(dfs$Index, "%Y-%m-%d"))
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
        p <- p + geom_line(aes_(x = quote(Index), y = quote(Value)),
                                    data = dfs, col = grDevices::gray(0.75), na.rm = TRUE)
        if (trend){
            p <- p + geom_smooth(data = dfs, method = "gam", formula = y ~ s(x, bs = "cs"),
                                          se = FALSE, na.rm = TRUE, lwd = 0.5, col = "black")
        }
    }

    return(p)
}

#' Returns 'pretty' units for use for plotting or printing
#'
#' @param unit a unit as recorded in the \code{\link{data.frame}}
#'     generated by \code{\link{generate_units}}.
#'
#' @details
#'
#' \code{prettifyUnits} is the vectorized version of \code{prettifyUnit}
#'
#' @examples
#' prettifyUnit("m_per_s")
#' prettifyUnit("rev_per_min")
#' prettifyUnits(c("rev_per_min", "ft_per_min"))
#'
#' @export
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

#' @rdname prettifyUnit
#' @export
prettifyUnits <- Vectorize(prettifyUnit)


#' Fortify a trackeRdata object for plotting with ggplot2
#'
#' @param model The \code{\link{trackeRdata}} object.
#' @param data Ignored.
#' @param melt Logical. Should the data be melted into long format
#'     instead of the default wide format?
#' @param ... Ignored.
#' @export
fortify.trackeRdata <- function(model,
                                data,
                                melt = FALSE,
                                ...){
    ret <- list()
    sports <- get_sport(model)
    for (i in seq_along(model)) {

        ret[[i]] <- zoo::fortify.zoo(model[[i]], melt = melt)
        ret[[i]]$SessionID <- i
        ret[[i]]$Sport <- sports[i]
        ## FIXME: add date identifier?
    }
    ret <- do.call("rbind", ret)
    return(ret)
}

#' Plot routes for training sessions
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
#' data('runs', package = 'trackeR')
#' plotRoute(runs, session = 4, zoom = 13)
#' plotRoute(runs, session = 4, zoom = 13, maptype = "hybrid")
#' ## multiple sessions
#' plotRoute(runs, session = c(1:5, 8:11), source = "google")
#' ## different zoom level per panel
#' plotRoute(runs, session = 6:7, source = "google", zoom = c(13, 14))
#' }
#' @export
plot_route <- function(x,
                       session = 1,
                       zoom = NULL,
                       speed = TRUE,
                       threshold = TRUE,
                       mfrow = NULL,
                       ...) {

    ## prep
    if (is.null(session)) {
        session <- seq_along(x)
    }

    if (!is.null(zoom)) {
        zoom <- rep(zoom, length.out = length(session))
    }

    sports <- get_sport(x)

    ## get prepared data.frame
    df <- prepare_route(x, session = session, threshold = threshold, ...)
    centers <- attr(df, "centers")

    if (speed) {
        speedRange <- range(df[["speed"]], na.rm = TRUE)
    }

    ## loop over sessions
    plotList <- vector("list", length(session))
    names(plotList) <- as.character(session)

    for (ses in session) {

        dfs <- df[df$SessionID == which(ses == session), , drop = FALSE]
        zooms <- if (is.null(zoom)) centers[centers$SessionID == ses, "zoom"] else zoom[which(ses == session)]

        ## get map
        map <- ggmap::get_map(location = c(lon = centers[centers$SessionID == ses, "centerLon"],
                                           lat = centers[centers$SessionID == ses, "centerLat"]),
                              zoom = zooms, ...)
        p <- ggmap::ggmap(map)

        ## add trace
        if (speed){
            p <- p + geom_segment(
                         aes_(x = quote(longitude0), xend = quote(longitude1),
                                       y = quote(latitude0), yend = quote(latitude1),
                                       color = quote(speed)),
                         data = dfs, lwd = 1, alpha = 0.8, na.rm = TRUE) +
                scale_color_gradient(limits = speedRange,
                                              guide = guide_colorbar(title = "Speed"))
        }
        else {
            p <- p + geom_segment(
                         aes_(x = quote(longitude0), xend = quote(longitude1),
                                       y = quote(latitude0), yend = quote(latitude1)),
                         data = dfs, lwd = 1, alpha = 0.8, na.rm = TRUE)
        }


        ## Extract legend from the first plot
        if (ses == session[1] & speed) {
            legend <- gtable::gtable_filter(ggplot_gtable(ggplot_build(p)), "guide-box")
        }

        p <- p + labs(title = paste(ses, ":", sports[ses]))
        plotList[[as.character(ses)]] <- p +  theme(legend.position = "none",
                                                             axis.title.x = element_blank(),
                                                             axis.title.y = element_blank())
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


#' Plot routes for training sessions
#'
#' Plot the route ran/cycled during training on an interactive map.
#' Internet connection is required to download the background map.
#' Icons are by Maps Icons Collection \url{https://mapicons.mapsmarker.com}
#'
#' @param x A object of class \code{\link{trackeRdata}}.
#' @param session A numeric vector of the sessions to be
#'     plotted. Defaults to all sessions.
#' @param threshold Logical. Should thresholds be applied?
#' @param ... Additional arguments passed on to
#'     \code{\link{threshold}}.
#' @examples
#' \dontrun{
#' data('runs', package = 'trackeR')
#' leafletRoute(runs, session = 23:24)
#' }
#' @export
leaflet_route <- function(x,
                         session = NULL,
                         threshold = TRUE,
                         ...) {

    if (is.null(session)) session <- seq_along(x)

    ## get prepared data.frame
    df <- prepare_route(x, session = session, threshold = threshold, ...)

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
    sumX <- summary(x)
    units <- getUnits(sumX)
    un <- collect_units(units, unit_reference_sport = attr(sumX, "unit_reference_sport"))
    distance_unit_from_pace <- strsplit(un$unit[un$variable == "pace"], split = "_per_")[[1]][2]

    popupText <- function(session, start = TRUE) {
        w <- which(sumX$session == session)
        avgPace <- floor(sumX$avgPace[w] * 100) / 100

        paste(
            paste("<b>", ifelse(start, "Start", "End"), "of session", session, "</b>"),
            paste(sumX$sessionStart[w], "-", sumX$sessionEnd[w]),
            paste("Distance:", round(sumX$distance[w], 2), un$unit[un$variable == "distance"]),
            paste("Duration:", round(as.numeric(sumX$duration[w]), 2), units(sumX$duration[w])),
            paste(paste0("Avg. pace (per 1 ", distance_unit_from_pace, "):"),
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


#' Prepare a \code{\link{data.frame}} for use in
#' \code{\link{leaflet_route}} and \code{\link{plot_route}}
#'
#' @param x a \code{\link{trackeRdata}} object.
#' @param session which session to prepare the
#'     \code{\link{data.frame}} for?
#' @param threshold if \code{TRUE} (default), then thresholds are
#'     applied to \code{x} prior to preparing the
#'     \code{\link{data.frame}}.
#' @param ... Addiditonal arguments to be passed to
#'     \code{\link{threshold}}.
#'
#'
#' @details
#'
#' To be used internally in mapping function and rarely by the user.
#'
#' @return
#'
#' A \code{data.frame} with variables \code{longitude},
#' \code{latitude}, \code{speed}, \code{SessionID}, \code{longitude0},
#' \code{longitude1}, \code{latitude0}, \code{latitude1}. The
#' observations are ordered according to the timestamp they have in
#' \code{x}. A suffix of 0 indicates 'start' and a suffix of 1
#' indicates 'end' at any given observation.
#' @export
prepare_route <- function(x,
                          session = 1,
                          threshold = TRUE,
                          ...) {
    ## get units for thresholds
    units <- getUnits(x)

    ## get sessions
    if (is.null(session)) session <- seq_along(x)
    x <- x[session]

    ## threshold
    if (threshold) {
        dots <- list(...)
        if (all(c("variable", "lower", "upper", "sport") %in% names(dots))) {
            th <- generate_thresholds(dots$variable, dots$lower, dots$upper, dots$sport)
        }
        else {
            ## default thresholds
            th <- generate_thresholds()
            th <- change_units(th, variable = units$variable, unit = units$unit, sport = units$sport)
        }
        ## apply thresholds
        x <- threshold(x, th$variable, th$lower, th$upper, th$sport)
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

    for (i in session) {
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

#' Timeline plot for \code{\link{trackeRdata}} objects.
#'
#' @inheritParams timeline
#' @rdname timeline
#' @export
timeline.trackeRdata  <- function(object,
                                  lims = NULL,
                                  ...) {
    df <- within(session_times(object), {
        day_s <- as.Date(sessionStart)
        day_e <-  as.Date(sessionEnd)
        time_s <- as.POSIXct(as.numeric(difftime(sessionStart, trunc(sessionStart, "days"),
                                                 units = "secs")),
                             origin = Sys.Date())
        time_e <- as.POSIXct(as.numeric(difftime(sessionEnd, trunc(sessionEnd, "days"),
                                                 units = "secs")),
                             origin = Sys.Date())
        sport <- get_sport(object)
    })

    if (!is.null(lims)) {
        lims <- as.POSIXct(paste(Sys.Date(), lims))
    }
    day_range <- data.frame(day = seq(min(df$day_s), max(df$day_s), by = "day"))
    p <- ggplot(df) +
        geom_segment(aes_(x = quote(time_s), xend = quote(time_e), y = quote(day_s), yend = quote(day_e), color = quote(sport)))
    ## take care of breaks, limits on the time axes and style of breakpoints
    p <- p + scale_x_datetime(date_labels = "%H:%m", date_breaks = "4 hour", limits = lims)
    p <- p + theme_bw() +
        theme(axis.text.x = element_text(angle = 50, hjust = 1),
              legend.position = "top") +
        xlab("Time") + ylab("Date")
    p
}


#' Ridgeline plots for \code{trackeRdata} objects
#'
#' @inheritParams distributionProfile
#' @param x A \code{trackeRdata} object.
#' @param smooth Logical. Should the concentration profiles be smoothed before plotting?
#' @param ... Currently not used.
#'
#' @examples
#'
#' \dontrun{
#' data('runs', package = 'trackeR')
#' ridges(runs)
#' }
#'
#' @export
ridges.trackeRdata <- function(x,
                               session = NULL,
                               what = "speed",
                               smooth = TRUE,
                               ...) {
    x <- distributionProfile(x, session = session, what = what)
    if (smooth) {
        x <- smoother(x, what = what, ...)
    }
    x <- concentrationProfile(x)
    ridges.conProfile(x, what = what, smooth = FALSE)
}

