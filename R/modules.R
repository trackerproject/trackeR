

generateGraphs <- function(x, group = c("total"), what, date = TRUE) {
  #x <- x()
  variable <- type <- NULL
  nsessions <- length(unique(x$session))
  ndates <- length(unique(x$sessionStart))
  units <- getUnits(x)

  ## subsets on variables and type
  dat <- fortify(x, melt = TRUE)
  if (!is.null(what)){
    dat <- subset(dat, variable %in% what)
  }
  if (!is.null(group)){
    dat <- subset(dat, type %in% group)
  }

  ## remove empty factor levels
  dat$variable <- factor(dat$variable)
  #dat$type <- factor(dat$type)

  ## clean up: if there are only NA observations for a variable, the (free) y-scale cannot be determined
  empty <- tapply(dat$value, dat$variable, function(x) all(is.na(x)))
  if (any(empty)) dat <- subset(dat, !(variable %in% names(empty)[empty]))

  ## single session
  if (nsessions < 2) {
    dat$sessionStart <- format(dat$sessionStart, format = "%Y-%m-%d")
    dat$session <- factor(dat$session)
  }

  ## x axis
  if (date) {
    dat$xaxis <- dat$sessionStart
    xlab <- "Date"
  } else {
    dat$xaxis <- dat$session
    xlab <- "Session"
  }


  return(dat)
}


plot_workouts <- function(dat, xaxis, yaxis, feature, name, units) {
    d <- event_data("plotly_selected")
    name <- switch(name,
                  "distance" = "Distance",
                  "duration" = "Duration",
                  "avgSpeed" = "Average Speed",
                  "avgPace" = "Average Pace",
                  "avgCadence" = "Average Cadence",
                  "avgPower" = "Average Power",
                  "avgHeartRate" = "Average Heart Rate",
                  "wrRatio" = "work-to-rest ratio"
    )
    #print(d)
    p <- plot_ly(dat, x = xaxis, y = yaxis, hoverinfo = 'text',
                 text = ~paste('Date:', format(sessionStart, format = "%Y-%m-%d"),
                               '\n', name, ':', round(value, 2), units)) %>%
      add_markers(key = dat$session, color = I('deepskyblue3')) %>% 
      add_lines(color = I('deepskyblue2'))
    if (length(d[['key']])>0){
      m <- dat[dat$session %in% d[['key']], ]
      p <- add_markers(p, data = m, color = I('darkorange3')) %>% 
        add_lines(data = m, color = I('darkorange2'))
    }
    y <- list(
      title = feature
    )
    x <- list(
      title = 'Date'
    )
    
    layout(p, dragmode = "select", showlegend = FALSE, yaxis = y, xaxis = x)
}

plot_map <- function(data, session){
  #session <- if (length(session) =! 0) session else 1:length(data$dataSet)
  leafletRoute(data, session)
}


lab_sum <- function(feature, data, whole_text = TRUE){
  feature <- as.character(feature)
  units <- getUnits(data)
  concept <- switch(feature, 'avgPace' = "pace", 'avgSpeed' = "speed",
                    'distance' = "distance", 'duration' = "duration",
                    'avgPower' = "power", 'avgCadence' = "cadence", 'avgHeartRate' = "heart.rate")
  thisunit <- units$unit[units$variable == concept]
  prettyUnit <- prettifyUnits(thisunit)
  if (whole_text == TRUE){
    ret <- switch(feature,
                  "distance" = paste0("Distance \n [", prettyUnit,"]"),
                  "duration" = paste0("Duration \n [", prettyUnit,"]"),
                  "avgSpeed" = paste0("Average Speed \n [", prettyUnit,"]"),
                  "avgPace" = paste0("Average Pace \n [", prettyUnit,"]"),
                  "avgCadence" = paste0("Average Cadence \n [", prettyUnit,"]"),
                  "avgPower" = paste0("Average Power \n [", prettyUnit,"]"),
                  "avgHeartRate" = paste0("Average Heart Rate \n [", prettyUnit,"]"),
                  "wrRatio" = "work-to-rest \n ratio"
    )
    
    ret
  } else {
    ret <- switch(feature,
                  "distance" = paste0("[", prettyUnit,"]"),
                  "duration" = paste0("[", prettyUnit,"]"),
                  "avgSpeed" = paste0("[", prettyUnit,"]"),
                  "avgPace" = paste0("[", prettyUnit,"]"),
                  "avgCadence" = paste0("[", prettyUnit,"]"),
                  "avgPower" = paste0("[", prettyUnit,"]"),
                  "avgHeartRate" = paste0("[", prettyUnit,"]"),
                  "wrRatio" = "work-to-rest ratio"
    )
    
    ret
  }

}

create_icon <- function(feature){
 icon <- switch(feature,
                "distance" = 'area-chart',
                "duration" = 'clock-o',
                "avgSpeed" = 'line-chart',
                "avgPace" = 'tachometer',
                "avgCadence" = 'spinner',
                "avgPower" = 'flash',
                "avgHeartRate" = 'heartbeat',
                "wrRatio" = 'fire'
  )
 
 icon
}




