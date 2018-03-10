server <- function(input, output, session) {
# Main object where all data is stored
data <- reactiveValues(summary=NULL, object=NULL, selectedSessions=NULL, hasData=NULL)
choices <- choices()
metrics <- metrics()
##################################################################################
# Get path to files
## Directory
shinyFiles::shinyDirChoose(
  input, "rawDataDirectory", roots = c(home = "~"),
  filetypes = c("gpx", "tcx", "db3", "json")
)
rawDataDirectory <- reactive({
  input$rawDataDirectory
})
raw_data_path <- reactive({
  home <- normalizePath("~")
  file.path(home, paste(
    unlist(rawDataDirectory()$path[-1]),
    collapse = .Platform$file.sep
  ))
})
## Processed data
shinyFiles::shinyFileChoose(
  input, "processedDataPath", roots = c(home = "~"),
  filetypes = c("rds", "rdata", "rda")
)
processed_data_file <- reactive({
  input$processedDataPath
})
processedDataPath <- reactive({
  home <- normalizePath("~")
  file.path(home, paste(
    unlist(processed_data_file()$files)[-1],
    collapse = .Platform$file.sep
  ))
})
## Home
home <- paste0(normalizePath("~"), "/")
##################################################################################
# Upload and process data
observeEvent(input$uploadButton, {
  # data$isCycling <- input$sportSelected == "cycling"
  if ((raw_data_path() == home) & (processedDataPath() == home)) {
    showModal(modalDialog(
      title = "trackeR dashboard message",
      div(tags$b(
        "Select a processed data image or a directory with raw datafiles",
        class = "warningMessage"
      )),
      easyClose = TRUE,
      size = "s"
    ))
  }
  else {
    processed_data <- raw_data <- NULL
    if (processedDataPath() != home) {
      processed_data <- readRDS(processedDataPath())
    }
    if (raw_data_path() != home) {
      raw_data <- callModule(
        module = readDirectory_shiny,
        id = "datafile",
        directory = raw_data_path(),
        timezone = "GMT", cycling = input$sportSelected == "cycling",
        correctDistances = FALSE
      )
    }
    # Remove duplicate sessions and create trackeRdata object
    data$object <- sort(unique(c.trackeRdata(processed_data, raw_data)), decreasing = FALSE)
    # Create trackeRdataSummary object
    data$summary <- summary(data$object)
    data$object <- threshold(data$object)
    data$object <- threshold(data$object, variable = 'distance', lower = 0, upper = 1000000)
    # Create trackeRdataSummary object
    data$summary <- summary(data$object, movingThreshold = 0.4)
    # Test if data in each element of trackeRdataSummary object
    data$hasData <- lapply(data$summary, function(session_summaries) {
      !all(is.na(session_summaries) | session_summaries == 0)
    })

      trackeR:::update_metrics_to_plot_workouts(session, choices, data$hasData)
      output$download_data <- trackeR:::download_handler(data)
      shinyjs::disable(selector = "#uploadButton")
  }
  data$selectedSessions <- data$summary$session
})
##################################################################################
# Change units
observeEvent(input$showModalUnits, {
  if (!is.null(data$object)) {
    trackeR:::show_change_unit_window(data)

  } else {
    trackeR:::show_warning_window()
   }
})
observeEvent(input$updateUnits, {
    data$object <- trackeR:::change_units(data, input, 'object')
    data$summary <- trackeR:::change_units(data, input, 'summary')
    removeModal()
})
##################################################################################
## Collapsing and opening timeline and summary boxes at top of page
# observeEvent(input$plotButton, {
#   shinyjs::js$collapse("timeline_plot")
#   shinyjs::js$collapse("summary")
# }, once = TRUE)

# observeEvent(input$plotSelectedWorkouts, {
#   shinyjs::js$collapse("timeline_plot")
#   shinyjs::js$collapse("summary")
# }, once = TRUE)
##################################################################################
## Main page
observeEvent({input$plotButton}, {
  if (is.null(data$object)) {
    trackeR:::show_warning_window()
  }
  else {
    output$timeline_plot <- plotly::renderPlotly({
      if (!is.null(data$summary)) {
        trackeR:::plot_timeline(data$summary[data$selectedSessions])
      }
    })
    trackeR:::create_option_box()
    trackeR:::create_summary_timeline_boxes()
    shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    ## DT
    output$summary <- trackeR:::render_summary_table(data)
    # Re-render all plots
    removeUI(selector = ".main_plots", immediate = TRUE, multiple = TRUE)
    trackeR:::create_map()
    output$map <- leaflet::renderLeaflet({

      trackeR:::shiny_plot_map(x = data$object, session = data$selectedSessions, sumX = data$summary)
    })
    trackeR:::create_summary_boxes()
    output$avgDistance_box <- trackeR:::render_summary_box('distance', 'Average distance', data)
    output$avgDuration_box <- trackeR:::render_summary_box('duration', 'Average duration', data)
    output$avgHeartRate_box <- trackeR:::render_summary_box('avgHeartRate', 'Average heart rate', data)
    output$avgPace_box <- trackeR:::render_summary_box('avgPace', 'Average pace', data)

    for (i in input$metricsSelected) {
      trackeR:::create_workout_plots(i)
    }
    lapply(input$metricsSelected, function(i) {
        output[[paste0(i, '_plot')]] <- plotly::renderPlotly({
          trackeR:::plot_workouts(sumX = data$summary, what = i)
        })
    })
    output$cond <- reactive({
      TRUE
    })
    outputOptions(output, "cond", suspendWhenHidden = FALSE)
  }
})
##################################################################################
observeEvent(input$resetButton, {
  shinyjs::js$reset_page()
})
##################################################################################
observeEvent(input$plotSelectedWorkouts, {
  shinyjs::addClass(selector = "body", class = "sidebar-collapse")
  # Test which metrics have data for selected sessions
  have_data_metrics_selected <- reactive({ !sapply(metrics, function(metric) {
    all(sapply(data$object[data$selectedSessions], function(x) all(is.na(x[, metric]))))
  })
  })

  for (i in c("pace", "heart.rate", "altitude", "work_capacity","speed")) {
    trackeR:::create_selected_workout_plot(id = i, data = data)
  }

  lapply(c("pace", "heart.rate", "altitude", "speed"), function(i) {
    output[[paste0(i, "_plot")]] <- plotly::renderPlotly({
      trackeR:::plot_selectedWorkouts(x=data$object, session=data$selectedSessions, what=i, sumX=data$summary)
    })
  })

  output$work_capacity_plot <- plotly::renderPlotly({
    if (input$sportSelected == "cycling" & all(is.na(data$summary$avgPower)) == TRUE) {
      removeUI(selector = "#work_capacity_plot")
    } else {
      trackeR:::plot_work_capacity(x=data$object, session=data$selectedSessions)
    }
  })

  trackeR:::create_zones_box(title='Time in Zones', inputId='zonesMetricsPlot',
             label='Select zone metrics to plot', plotId='zonesPlotUi',
             choices = metrics[have_data_metrics_selected()])
  trackeR:::create_profiles_box(title='Concentration profiles', inputId='profileMetricsPlot',
             label='Select profile metrics to plot', plotId='concentration_profiles',
             choices = metrics[have_data_metrics_selected()])


  # update_metrics_to_plot_selected_workouts(id = 'zonesMetricsPlot', session, metrics, have_data_metrics_selected())
  #
  # update_metrics_to_plot_selected_workouts(id = 'profileMetricsPlot', session, metrics, have_data_metrics_selected())

  ## Render UI for time in zones plot
  output$zonesPlotUi <- renderUI({
    shiny::req(input$zonesMetricsPlot)
    shinycssloaders::withSpinner(plotly::plotlyOutput("zones_plot", width = "100%",
                                                      height = trackeR:::calculate_plot_height(input$zonesMetricsPlot)), size = 2)
  })
  ## Render actual plot
  output$zones_plot <- plotly::renderPlotly({
    trackeR:::plot_zones(x = data$object, session = data$selectedSessions, what = input$zonesMetricsPlot,
               n_zones = as.numeric(input$n_zones))
  })

  ## Render UI for concentration profiles
  output$concentration_profiles <- renderUI({
    shiny::req(input$profileMetricsPlot)
    shinycssloaders::withSpinner(plotly::plotlyOutput("conc_profiles_plots", width = "auto",
                                                      height = trackeR:::calculate_plot_height(input$profileMetricsPlot)), size = 2)
  })

  ## Render actual plot
  output$conc_profiles_plots <- plotly::renderPlotly({

    trackeR:::plot_concentration_profiles(x = data$object, session = data$selectedSessions, what = input$profileMetricsPlot)
  })
}, once=TRUE)

##################################################################################
observeEvent(input$return_to_main_page, {
shinyjs::addClass(selector = "body", class = "sidebar-collapse")
output$cond <- reactive({ TRUE })
})
observeEvent(input$plotSelectedWorkouts, {
shinyjs::addClass(selector = "body", class = "sidebar-collapse")
output$cond <- reactive({ FALSE })
})
##################################################################################
}

