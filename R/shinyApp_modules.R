#' Generate a formatted data frame for \code{plot_workouts()} for a given variable.
#'
#' @param x An object of class \code{trackeRdataSummary}.
#' @param what A character of a variable name to be plotted (e.g. "avgHeartRate").

generate_graph_data <- function(x, what) {
  #x <- x()
  date <- TRUE
  group <- c("total")
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

#' A time-series plotly graph for a given variable (e.g. "avgHeartRate").
#'
#' @param dat A dataframe generated using \code{generate_graph_data()}.
#' @param feature A character for y-axis title generated using \code{lab_sum()}.
#' @param name A character for the feature to be plotted (e.g. "avgHeartRate").
#' @param units A character of the unit of measurement for the given variable (e.g. "[bmp]") generated using \code{lab_sum()}.

plot_workouts <- function(dat, feature, name, units) {
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
    p <- plot_ly(dat, x = ~xaxis, y = ~value, hoverinfo = 'text',
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

    layout(p, dragmode = "select", showlegend = FALSE, yaxis = y, xaxis = x, margin = list(l=80, b=50, pad=0))
}

# plot_map <- function(data, session){
#   #session <- if (length(session) =! 0) session else 1:length(data$dataSet)
#   leafletRoute(data, session)
# }


#' Generate a character of formatted units, either only the unit (e.g "[bpm]") or whole text (e.g. "Heart Rate \n [bpm]").
#'
#' @param feature A character representing the feature whose units we want to generate.
#' @param data An object of class \code{trackeRdataSummary} or \code{trackeRdata}.
#' @param whole_text Generate only unit (e.g "[bpm]") or whole text (e.g. "Heart Rate \n [bpm]").
#' @param transform_feature If TRUE, epected format of \code{feature} is such as "avgCadence", "avgPower". If FALSE, expected format is "pace", "cadence", "heart.rate" or "altitude".

lab_sum <- function(feature, data, whole_text = TRUE, transform_feature = TRUE){
  feature <- as.character(feature)
  units <- getUnits(data)
  if(transform_feature == TRUE){
    concept <- switch(feature, 'avgPace' = "pace", 'avgSpeed' = "speed",
                      'distance' = "distance", 'duration' = "duration",
                      'avgPower' = "power", 'avgCadence' = "cadence", 'avgHeartRate' = "heart.rate"
                      )
  } else {
    concept <- feature
  }
  thisunit <- units$unit[units$variable == concept]
  prettyUnit <- prettifyUnits(thisunit)
  if (whole_text == TRUE){
    if(transform_feature == TRUE){
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
    } else {
      ret <- switch(feature,
                    "pace" = paste0("Pace \n [", prettyUnit,"]"),
                    "cadence" = paste0("Cadence \n [", prettyUnit,"]"),
                    "heart.rate" = paste0("Heart Rate \n [", prettyUnit,"]"),
                    "altitude" = paste0("Altitude \n [", prettyUnit,"]")
                    )
    }

    ret
  } else {
    if(transform_feature == TRUE){
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
    } else {
      ret <- switch(feature,
                    "pace" = paste0("[", prettyUnit,"]"),
                    "cadence" = paste0("[", prettyUnit,"]"),
                    "heart.rate" = paste0("[", prettyUnit,"]"),
                    "altitude" = paste0("[", prettyUnit,"]")
                    )
    }
    ret
  }

}


#' Generate an icon for a given feature.
#'
#' @param feature A character representing the feature whose units we want to generate.

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





