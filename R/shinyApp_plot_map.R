#' Plot all selected workouts on an interactive leaflet map.
#'
#' @param x An object of class \code{\link{trackeRdata}}.
#' @param sumX An object of class \code{\link{trackeRdataSummary}}.
#' @param session A numeric vector of the sessions to be plotted. Defaults to
#'     all sessions.
#' @param threshold Logical. Should thresholds be applied?
#' @param shiny_version Logical. Whether plot shiny version of map or TrackeR version.
#'
plot_map <- function(x, sumX, session = NULL, threshold = TRUE, shiny_version = TRUE, ...){
    if (is.null(session)) session <- seq_along(x)
    ## get prepared data.frame
    df <- prepRoute(x, session = session, threshold = threshold, ...)
    ## prepare popups
    units <- getUnits(x)
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
    if (shiny_version) {
        # create icons
        startIcon <- leaflet::makeIcon(
        iconUrl = system.file("icons", "green_marker.png", package = "trackeR"),
        iconWidth = 32, iconHeight = 37, iconAnchorX = 16, iconAnchorY = 37
        )
        finishIcon <- leaflet::makeIcon(
        iconUrl = system.file("icons", "red_marker.png", package = "trackeR"),
        iconWidth = 32, iconHeight = 37, iconAnchorX = 16, iconAnchorY = 37
        )
        p <- leaflet::leaflet() %>% leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron)
    } else {

        ## prepare markers
        startIcon <- leaflet::makeIcon(
            iconUrl = system.file("icons", "start.png", package = "trackeR"),
            iconWidth = 32, iconHeight = 37, iconAnchorX = 16, iconAnchorY = 37
        )
        finishIcon <- leaflet::makeIcon(
            iconUrl = system.file("icons", "finish.png", package = "trackeR"),
            iconWidth = 32, iconHeight = 37, iconAnchorX = 16, iconAnchorY = 37
        )
        ## get map
        p <- leaflet::leaflet()
        p <- leaflet::addTiles(p, group = "OSM (default)")
        p <- leaflet::addProviderTiles(p, "Stamen.Toner", group = "Toner")
        p <- leaflet::addProviderTiles(p, "Stamen.TonerLite", group = "Toner Lite")
        ## add control panel
        p <- leaflet::addLayersControl(p, baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
                              overlayGroups = paste("Session:", session),
                              options = leaflet::layersControlOptions(collapsed = FALSE))
    }
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

    return(p)
}



