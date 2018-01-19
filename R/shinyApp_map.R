## Plot all selected workouts on an interactive leaflet map.
##
## @param x An object of class \code{trackeRdata}.
## @param session A vector of selected sessions.
## @param data_summary An object of class \code{trackeRdataSummary}.

shiny_plot_map <- function(x, session, data_summary){
  data <- x
  sessions <- session
  session <- session[sapply(data[sessions], function(x){ !all(is.na(x$latitude))})]
  threshold = TRUE

  if (is.null(session)) session <- seq_along(x)
  ## get prepared data.frame
  df <- prepRoute(x, session = session, threshold = threshold)
  # create icons
  startIcon <- leaflet::makeIcon(
    iconUrl = system.file("icons", "green_marker.png", package = "trackeR"),
    iconWidth = 32, iconHeight = 37, iconAnchorX = 16, iconAnchorY = 37
  )
  finishIcon <- leaflet::makeIcon(
    iconUrl = system.file("icons", "red_marker.png", package = "trackeR"),
    iconWidth = 32, iconHeight = 37, iconAnchorX = 16, iconAnchorY = 37
  )

  ## prepare popups
  units <- getUnits(x)
  sumX <- data_summary

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

  p <- leaflet::leaflet() %>% leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron)
  #p <- leaflet() %>% addTiles()
  ## add trace + markers + popups
  for (i in session){
    dfi <- df[df$SessionID == which(i == session), , drop = FALSE]
    p <- leaflet::addPolylines(p, group = paste("Session:", i),
                               lng = dfi$longitude, lat = dfi$latitude,
                               popup = popupText(session = i, start = TRUE))
    p <- leaflet::addMarkers(p, group = paste("Session:", i),
                             lng = dfi$longitude[1], lat = dfi$latitude[1],
                             popup = popupText(session = i, start = TRUE), icon = startIcon)
    p <- leaflet::addMarkers(p, group = paste("Session:", i),
                             lng = dfi$longitude[nrow(dfi)], lat = dfi$latitude[nrow(dfi)],
                             popup = popupText(session = i, start = FALSE), icon = finishIcon)
  }
  return(p)
}



