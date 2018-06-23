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
  unused_variables <- c("latitude", "longitude", "heart.rate", "duration", "temperature")
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
    # TODO decide on whether to implement a power plot as well, give that
    # work capacity plot already included ("Power" = "power").
    "Pace" = "pace"
  )
}

#' Update panel with metrics to plot
#' @param session A shiny object.
#' @param choices A vector. A list of features to plot, see \code{\link{choices}}.
#' @param has_data A vector with boolean expressions representing which features have data. 
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
#' @param id A character. The ID of the input.
#' @param session A shiny object.
#' @param metrics A vector.  A list of features to plot, see \code{\link{metrics}}.
#' @param has_data A vector with boolean expressions representing which features have data.
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
#' @param data An object of class \code{reactivevalues}.
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
#' @param metrics A vector of metrics that will be plotted. 
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
#' @param data An object of class \code{reactivevalues}.
classify_sessions_by_sport <- function(data) {
  filepath <- system.file('inst/extdata/sport_classification_train.csv', package = 'trackeR')
  sport_classification_train <- read.csv(filepath)
  n_train <- nrow(sport_classification_train)
  merged_df <- rbind(sport_classification_train[,c('avgPaceMoving','distance')],
                     data.frame(avgPaceMoving = data$summary$avgPaceMoving,
                                distance = data$summary$distance))
  merged_df[is.na(merged_df)] <- 0
  merged_df <- scale(merged_df)
  classified_sports <- class::knn(
    merged_df[1:n_train,], merged_df[(n_train + 1):nrow(merged_df),], 
    sport_classification_train[,'sport'], k=5
    )

  sports <- sport(data$object)
  for(i in c(1:length(sports))){
    if(is.na(sports[i])){
      sports[i] <- switch(as.vector(classified_sports)[i],
                          'Swim' = 'swimming',
                          'Ride' = 'cycling',
                          'Run' = 'running')
    }
  }
  attr(data$object, 'sport') <- sports
}


#' Process \code{trackeRdata} object by: setting thresholds to remove wrong values, change units, set a moving threshold and test which variables contain data
#' @param data An object of class \code{reactivevalues}.
process_dataset <- function(data){
  data$object <- threshold(data$object)
  data$object <- threshold(data$object, variable = 'distance', lower = 0, upper = 500000)
  data$object <- changeUnits(data$object, variable = c("distance", 'pace', 'speed'),
                              unit = c("km", 'min_per_km', 'km_per_h'))
  # Create trackeRdataSummary object
  data$summary <- summary(data$object, movingThreshold = 0.4)
  data$summary <- changeUnits(data$summary, variable = c('duration'),
                              unit = c('h'))
  # Test if data in each element of trackeRdataSummary object
  data$hasData <- lapply(data$summary, function(session_summaries) {
    !all(is.na(session_summaries) | session_summaries == 0)
  })
}


# Set up a button to have an animated loading indicator and a checkmark
# for better user experience
# Need to use with the corresponding `withBusyIndicator` server function
withBusyIndicatorUI <- function(button) {
  id <- button[['attribs']][['id']]
  div(
    `data-for-btn` = id,
    button,
    span(
      class = "btn-loading-container",
      shinyjs::hidden(
        img(src = "ajax-loader-bar.gif", class = "btn-loading-indicator"),
        icon("check", class = "btn-done-indicator")
      )
    ),
    shinyjs::hidden(
      div(class = "btn-err",
          div(icon("exclamation-circle"),
              tags$b("Error: "),
              span(class = "btn-err-msg")
          )
      )
    )
  )
}

# Call this function from the server with the button id that is clicked and the
# expression to run when the button is clicked
withBusyIndicatorServer <- function(buttonId, expr) {
  # UX stuff: show the "busy" message, hide the other messages, disable the button
  loadingEl <- sprintf("[data-for-btn=%s] .btn-loading-indicator", buttonId)
  doneEl <- sprintf("[data-for-btn=%s] .btn-done-indicator", buttonId)
  errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
  shinyjs::disable(buttonId)
  shinyjs::show(selector = loadingEl)
  shinyjs::hide(selector = doneEl)
  shinyjs::hide(selector = errEl)
  on.exit({
    shinyjs::enable(buttonId)
    shinyjs::hide(selector = loadingEl)
  })

  # Try to run the code when the button is clicked and show an error message if
  # an error occurs or a success message if it completes
  tryCatch({
    value <- expr
    shinyjs::show(selector = doneEl)
    shinyjs::delay(2000, shinyjs::hide(
      selector = doneEl, anim = TRUE, animType = "fade",
      time = 0.5
    ))
    value
  }, error = function(err) {
    errorFunc(err, buttonId)
  })
}

# When an error happens after a button click, show the error
errorFunc <- function(err, buttonId) {
  errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
  errElMsg <- sprintf("[data-for-btn=%s] .btn-err-msg", buttonId)
  errMessage <- gsub("^ddpcr: (.*)", "\\1", err$message)
  shinyjs::html(html = errMessage, selector = errElMsg)
  shinyjs::show(selector = errEl, anim = TRUE, animType = "fade")
}

appCSS <- "
.btn-loading-container {
margin-left: 10px;
font-size: 1.2em;
}
.btn-done-indicator {
color: green;
}
.btn-err {
margin-top: 10px;
color: red;
}
.warningMessage {
                          font-size: 20px;
}
hr {
border-top: 1px solid;
}
a#download_data {
color: #333;
}
"

# Update map based on current selection
update_map <- function(plot_df, session, data){
  plotly::plotlyProxy("map", session) %>% plotly::plotlyProxyInvoke(
    "restyle",
    list(line.color = "rgba(238, 118, 0, 1)"), as.list(which(data$sessions_map %in% data$selectedSessions) - 1)
  )
  plotly::plotlyProxy("map", session) %>% plotly::plotlyProxyInvoke(
    "restyle",
    list(line.fillcolor = "rgba(238, 118, 0, 1)"), as.list(which(data$sessions_map %in% data$selectedSessions) - 1)
  )
  plotly::plotlyProxy("map", session) %>% plotly::plotlyProxyInvoke(
    "restyle",
    list(line.color = "rgba(0, 154, 205, 1)"), as.list(which(!(data$sessions_map %in% data$selectedSessions)) - 1)
  )
  plotly::plotlyProxy("map", session) %>% plotly::plotlyProxyInvoke(
    "restyle",
    list(line.fillcolor = "rgba(0, 154, 205, 1)"), as.list(which(!(data$sessions_map %in% data$selectedSessions)) - 1)
  )
  plotly::plotlyProxy("map", session) %>% plotly::plotlyProxyInvoke(
    "relayout",
    list(mapbox.zoom = 7)
  )
  plotly::plotlyProxy("map", session) %>% plotly::plotlyProxyInvoke(
    "relayout",
    list(mapbox.center = list(
      lat = median(plot_df$latitude),
      lon = median(plot_df$longitude)
    ))
  )
}

#' #' Calculate plot height for work capacity
#' calculate_plot_height_work_capacity <- function(name, data) {
#'   if (name != 'work_capacity') {
#'     "250px"
#'   } else {
#'     sports <- unique(sport(data$object[data$selectedSessions]))
#'     # Work capacity only for running and cycling
#'     sports <- intersect(c('running', 'cycling'), sports)
#'     paste0(250 * length(sports), "px")
#'   }
#' }

#' Plot work capacities for each sport
#' @param x An object of class \code{\link{trackeRdata}}.
#' @param session A numeric vector of the sessions to be used, defaults to all sessions.
#' @param cp A numeric. Critical power/speed, i.e., the power/speed which can be maintained for longer period of time.
plot_work_capacities <- function(x, session, cp) {
  sports <- unique(sport(x[session]))
  # Work capacity only for running and cycling
  sports <- intersect(c('running', 'cycling'), sports)
  if (length(sports) == 1) {
    return(plot_work_capacity(x = x, session = session, cp = cp))
  } else {
    cycling_sessions <- session[sport(x[session]) == 'cycling']
    plot_cycling <- plot_work_capacity(x = x, session = c(cycling_sessions, -1),
                                                 cp = cp)
    running_sessions <- session[sport(x[session]) == 'running']
    plot_running <- plot_work_capacity(x = x, session = running_sessions,
                                                 cp = cp)
    plots <- do.call(plotly::subplot, c(
      list(plot_cycling, plot_running), nrows = 2,
      margin = 0.05, shareY = FALSE, titleX = TRUE, titleY = TRUE
    ))
    return(plots)
  }
}

#' Show a modal window to inform a user that no data was selected 
show_warning_no_data_selected <- function() {
  showModal(modalDialog(
    title = "trackeR dashboard message",
    div(tags$b(
      "Select a processed data image or a directory with raw datafiles",
      class = "warningMessage"
    )),
    size = "s",
    footer = tagList(
      modalButton("Cancel"),
      actionButton("uploadSampleDataset", "Upload sample dataset")
    )
  ))
}

#' Classify sessions by sport, process dataset, generate download handler,
#' generate selected sessions object, update what metrics are available 
#' to plot and other minor actions.
#' @param data An object of class \code{reactivevalues}.
#' @param output A shiny object.
#' @param session A shiny object.
#' @param choices A vector. See \code{\link{choices}}. 
generate_objects <- function(data, output, session, choices) {
  process_dataset(data)
  ## Update sport attribute of data$object with classified sports
  classify_sessions_by_sport(data)
  output$download_data <- download_handler(data)
  shinyjs::disable(selector = "#processedDataPath")
  data$selectedSessions <- data$summary$session
  data$sessions_map <- rep(data$summary$session, times = 1, each = 2)
  shinyjs::click("createDashboard")
  update_metrics_to_plot_workouts(session, choices, data$hasData)
}

#' Currently available sports in the trackeRdashboard.
sports_options <- c(
  "Running" = "running",
  "Cycling" = "cycling",
  "Swimming" = "swimming"
)

#' Test whether we can plot work capacity for at least one of cycling or running.
#' @param data An object of class \code{reactivevalues}.
test_work_capacity <- function(data) {
  selected_sports <- unique(sport(data$object[data$selectedSessions]))
  is_data_power <- !all(sapply(data$object[data$selectedSessions], function(x){ 
    all((is.na(x[, "power"])) | (x[, "power"] == 0))
    }
  ))
  # Test for power if cycling
  if (('cycling' %in% selected_sports) & (is_data_power)) {
    cycling <- 'cycling'
  } else {
    cycling <- NULL
  }
  # Test if running selected
  if ('running' %in% selected_sports) {
    running <- 'running'
  } else {
    running <- NULL
  }
  return(c(cycling, running))
}