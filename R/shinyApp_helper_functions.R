#' Generate a character of formatted units, either only the unit (e.g "[bpm]") or whole text (e.g. "Heart Rate [bpm]").
#'
#' @param feature A character representing the feature whose units we want to generate.
#' @param data An object of class \code{trackeRdataSummary} or \code{trackeRdata}.
#' @param whole_text Generate only unit (e.g "[bpm]") or whole text (e.g. "Heart Rate [bpm]").
#' @param transform_feature If TRUE, expected format of \code{feature} is such as "avgCadence", "avgPower". If FALSE, expected format is "pace", "cadence", "heart.rate" or "altitude".
lab_sum <- function(feature, data, whole_text = TRUE, transform_feature = TRUE) {
  feature <- as.character(feature)
  units <- getUnits(data)
  if (transform_feature) {
    concept <- switch(feature, "avgPace" = "pace", "avgSpeed" = "speed",
      "distance" = "distance", "duration" = "duration",
      "avgPower" = "power", "avgCadence" = "cadence", "avgHeartRate" = "heart.rate"
    )
  }
  else {
    concept <- feature
  }
  thisunit <- units$unit[units$variable == concept]
  prettyUnit <- prettifyUnits(thisunit)
  if (whole_text) {
    if (transform_feature) {
      ret <- switch(feature,
        "distance" = paste0("Distance \n[", prettyUnit, "]"),
        "duration" = paste0("Duration \n[", prettyUnit, "]"),
        "avgSpeed" = paste0("Average Speed \n[", prettyUnit, "]"),
        "avgPace" = paste0("Average Pace \n[", prettyUnit, "]"),
        "avgCadence" = paste0("Average Cadence \n[", prettyUnit, "]"),
        "avgPower" = paste0("Average Power \n[", prettyUnit, "]"),
        "avgHeartRate" = paste0("Average Heart Rate \n[", prettyUnit, "]"),
        "wrRatio" = "work-to-rest \n ratio"
      )
    }
    else {
      ret <- switch(feature,
        "pace" = paste0("Pace \n[", prettyUnit, "]"),
        "cadence" = paste0("Cadence \n[", prettyUnit, "]"),
        "heart.rate" = paste0("Heart Rate \n[", prettyUnit, "]"),
        "altitude" = paste0("Altitude \n[", prettyUnit, "]"),
        "speed" = paste0("Speed \n[", prettyUnit, "]")
      )
    }
    ret
  }
  else {
    if (transform_feature) {
      ret <- switch(feature,
        "distance" = prettyUnit,
        "duration" = prettyUnit,
        "avgSpeed" = prettyUnit,
        "avgPace" = prettyUnit,
        "avgCadence" = prettyUnit,
        "avgPower" = prettyUnit,
        "avgHeartRate" = prettyUnit,
        "wrRatio" = "work-to-rest ratio"
      )
    }
    else {
      ret <- switch(feature,
        "pace" = prettyUnit,
        "cadence" = prettyUnit,
        "heart.rate" = prettyUnit,
        "altitude" = prettyUnit,
        "speed" = prettyUnit
      )
    }
    ret
  }
}

#' Generate an icon for a given feature.
#'
#' @param feature A character representing the feature whose units we want to generate.
create_icon <- function(feature) {
  icon <- switch(feature,
    "distance" = "area-chart",
    "duration" = "clock-o",
    "avgSpeed" = "line-chart",
    "avgPace" = "tachometer",
    "avgCadence" = "spinner",
    "avgPower" = "flash",
    "avgHeartRate" = "heartbeat",
    "wrRatio" = "fire"
  )
  icon
}

#' Get units of measurement for a given feature
#'
#' @param feature A character for the feature whose units we want to access, for example 'altitude', 'distance',...
#' @param data An object of class \code{reactivevalues}.
get_selected_units <- function(feature, data) {
  getUnits(data$summary)$unit[getUnits(data$summary)$variable %in% feature]
}


#' Change units of variables.
#'
#' @param data An object of class \code{reactivevalues}.
#' @param input An object of class \code{reactivevalues}.
#' @param object A character of either 'summary' or 'object' to specify which objects' units to change.
change_units <- function(data, input, object) {
  unused_variables <- c("latitude", "longitude", "heart.rate", "duration")
  allUnits <- getUnits(data$object)$variable[!(getUnits(data$object)$variable %in% unused_variables)]

  units <- c()
  for (i in allUnits) {
    units <- c(units, input[[paste0(i, "Units")]])
  }
  data_updated <- changeUnits(data[[object]], variable = allUnits, unit = units)
  if(object == 'summary'){
    data_updated <- changeUnits(data_updated , variable='duration', unit=input[['durationUnits']])
  }
  return(data_updated)
}

#' Generate choices for plots
choices <- function() {
  c(
    "Distance" = "distance",
    "Duration" = "duration",
    "Average speed" = "avgSpeed",
    "Average pace" = "avgPace",
    "Average cadence" = "avgCadence",
    "Average power" = "avgPower",
    "Average heart rate" = "avgHeartRate",
    "Work to rest ratio" = "wrRatio"
    )
}

#' Generate metrics to test if they have data
metrics <- function() {
   c(
    "Heart Rate" = "heart.rate",
    "Altitude" = "altitude",
    "Speed" = "speed",
    "Cadence" = "cadence",
    "Power" = "power",
    "Pace" = "pace"
  )
}

#' Update panel with metrics to plot
update_metrics_to_plot_workouts <- function(session, choices, has_data) {
  updateSelectizeInput(
    session = session,
    inputId = "metricsSelected",
    choices = choices[sapply(choices, function(x) { has_data[[x]] })],
    server = TRUE,
    selected = c("distance", "duration", 'avgSpeed')
  )
}

#' Update metrics to plot for Work capacity and time in zones
update_metrics_to_plot_selected_workouts <- function(id, session, metrics, has_data) {
  updateSelectizeInput(
    session = session,
    inputId = id,
    choices = metrics[has_data],
    server = TRUE,
    selected = c("speed")
  )
}

#' Download handler
download_handler <- function(data) {
  downloadHandler(
      filename = function() {
        paste0("trackeR-dashboard-data", Sys.Date(), ".rds")
      },
      content = function(file) {
        saveRDS(data$object, file)
      }
    )
}

#' Show warning window when no data uploaded
show_warning_window <- function() {
  showModal(modalDialog(
  title = "trackeR dashboard message",
  div(tags$b(
    "Load processed and/or raw data",
    class = "warningMessage"
  )),
  easyClose = TRUE,
  size = "s"
  ))
}

#' Calculate plot height for either time in zones or work capacity
calculate_plot_height <- function(metrics) {
  paste0(250 * length(metrics), "px")
}

#' Get Javascript code for reseting page and collapsing boxes
get_javascript <- function() {
  "
    shinyjs.collapse = function(boxid) {
    $('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
    };
    // shinyjs.resetClick = function() { Shiny.onInputChange('.clientValue-plotly_selected-A', 'null'); }
    shinyjs.reset_page = function() { location.reload(); }
  "
}

#' Classify sessions by sport using the KNN model and 'sport_classification_train' dataset as a training set
classify_sessions_by_sport <- function(data) {
  data(sport_classification_train)
  n_train <- nrow(sport_classification_train)
  merged_df <- rbind(sport_classification_train[,c('avgPaceMoving','distance')],
                     data.frame(avgPaceMoving = data$summary$avgPaceMoving,
                                distance = data$summary$distance))
  merged_df[is.na(merged_df)] <- 0
  merged_df <- scale(merged_df)
  data$classification <- class::knn(
    merged_df[1:n_train,], merged_df[(n_train + 1):nrow(merged_df),], sport_classification_train[,'sport'], k=5)
}


#' Process \code{trackeRdata} object by: setting thresholds to remove wrong values, change units, set a moving threshold and test which variables contain data
process_dataset <- function(data){
  data$object <- threshold(data$object)
  data$object <- threshold(data$object, variable = 'distance', lower = 0, upper = 500000)
  data$object <- changeUnits(data$object, variable = c("distance", 'pace'),
                              unit = c("km", 'min_per_km'))
  # Create trackeRdataSummary object
  data$summary <- summary(data$object, movingThreshold = 0.4)
  data$summary <- changeUnits(data$summary, variable = c('duration'),
                              unit = c('h'))
  # Test if data in each element of trackeRdataSummary object
  data$hasData <- lapply(data$summary, function(session_summaries) {
    !all(is.na(session_summaries) | session_summaries == 0)
  })
}


