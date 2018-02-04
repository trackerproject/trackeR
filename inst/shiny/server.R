server <- function(input, output, session) {
  # Main object where all data is stored
  data <- reactiveValues()
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
  data$isCycling <- input$sportSelected == "cycling"
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
        timezone = "GMT", cycling = data$isCycling,
        correctDistances = TRUE
      )
    } 
    # Remove duplicate sessions and create trackeRdata object
    data$object <- sort(unique(c.trackeRdata(processed_data, raw_data)), decreasing = FALSE)
    # Create trackeRdataSummary object
    data$summary <- summary(data$object)
    # Test if data in each element of trackeRdataSummary object
    data$hasData <- lapply(data$summary, function(session_summaries) {
      !all(is.na(session_summaries) | session_summaries == 0)
    })
    update_metrics_to_plot_workouts(session, choices, data$hasData)
    output$download_data <- download_handler(data)
  }
  data$selectedSessions <- data$summary$session
}, once = TRUE)
##################################################################################
# Change units
observeEvent(input$changeUnits, {
  if (!is.null(data$object)) {
    show_change_unit_window(data)
  } else {
    show_warning_window()
   }
})
observeEvent(input$updateUnits, {
    data$object <- change_units(data, input, 'object')
    data$summary <- change_units(data, input, 'summary')
    removeModal()
})
##################################################################################
## Collapsing and opening timeline and summary boxes at top of page
observeEvent(input$plotButton, {
  shinyjs::js$collapse("timeline_plot")
  shinyjs::js$collapse("summary")
}, once = TRUE)

observeEvent(input$plotSelectedWorkouts, {
  shinyjs::js$collapse("timeline_plot")
  shinyjs::js$collapse("summary")
}, once = TRUE)
##################################################################################
## Main page
observeEvent({input$plotButton}, {
  if (is.null(data$object)) {
    show_warning_window()
  }
  else {
    output$timeline_plot <- renderPlot({
      if (!is.null(data$summary)) {
        timeline(data$summary[data$selectedSessions])
      }
    })
    # Re-render all plots
    removeUI(selector = ".plots", immediate = TRUE, multiple = TRUE)
    create_map()
    create_summary_boxes()
    for (i in input$metricsSelected) {
      create_workout_plots(i)
    }
    output$averageDistance <- render_summary_box('distance', 'Average distance', data)
    output$averageDuration <- render_summary_box('duration', 'Average duration', data)
    output$averageHeartRate <- render_summary_box('avgHeartRate', 'Average heart rate', data)
    output$averagePace <- render_summary_box('avgPace', 'Average pace', data)

    output$map <- leaflet::renderLeaflet({
      shiny_plot_map(x = data$object, session = data$selectedSessions, sumX = data$summary)
    })

    lapply(input$metricsSelected, function(i) {
        output[[paste0(i)]] <- plotly::renderPlotly({
          plot_workouts(sumX = data$summary, what = i)
        })
    })

    ## DT
    output$summary <- render_summary_table(data)
    # XXX:
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
  # Test which metrics have data for selected sessions
  have_data_metrics_selected <- reactive({ !sapply(metrics, function(metric) {
    all(sapply(data$object[data$selectedSessions], function(x) all(is.na(x[, metric]))))
  })
  })

  ### XXX here use to create go-back button
  output$cond <- reactive({
    FALSE
  })

  for (i in c("pace", "heart.rate", "altitude", "work_capacity")) {
    create_selected_workout_plot(id = i, data = data)
  }

  lapply(c("pace", "heart.rate", "altitude"), function(i) {
    output[[paste0("plot_", i)]] <- plotly::renderPlotly({
      plot_selectedWorkouts(x=data$object, session=data$selectedSessions, what=i, sumX=data$summary)
    })
  })

  output[[paste0("plot_", "work_capacity")]] <- plotly::renderPlotly({
    if (data$isCycling & all(is.na(data$summary$avgPower)) == TRUE) {
      removeUI(selector = paste0("#", "work_capacity"))
    } else {
      plot_work_capacity(x=data$object, session=data$selectedSessions)
    }
  })

  create_time_in_zones_plot()
  create_concentration_profile_plot()
  update_metrics_to_plot_selected_workouts(id = 'zonesMetricsPlot', session, metrics, have_data_metrics_selected())
  update_metrics_to_plot_selected_workouts(id = 'profileMetricsPlot', session, metrics, have_data_metrics_selected())

  ## Render UI for time in zones plot
  output$time_in_zones <- renderUI({
    shinycssloaders::withSpinner(plotly::plotlyOutput("time_in_zone_plots", width = "auto",
                                                      height = calculate_plot_height(input$zonesMetricsPlot)), size = 2)
  })
  ## Render actual plot
  output$time_in_zone_plots <- plotly::renderPlotly({
    plot_zones(x = data$object, session = data$selectedSessions, what = input$zonesMetricsPlot)
  })

  ## Render UI for concentration profiles
  output$concentration_profiles <- renderUI({
    shinycssloaders::withSpinner(plotly::plotlyOutput("concentration_profiles_plots", width = "auto",
                                                      height = calculate_plot_height(input$profileMetricsPlot)), size = 2)
  })
  ## Render actual plot
  output$concentration_profiles_plots <- plotly::renderPlotly({
    plot_concentration_profiles(x = data$object, session = data$selectedSessions, what = input$profileMetricsPlot)
  })
})
}
