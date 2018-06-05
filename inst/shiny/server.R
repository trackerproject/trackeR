# TRUE only for a live version
live_version <- FALSE

# Load packages for a live version
if (live_version == TRUE) {
  library(shiny)
  library(shinyjs)
  library(leaflet)
  library(plotly)
  library(shinycssloaders)
  library(trackeR)
}
# Set token for Mapbox
Sys.setenv("MAPBOX_TOKEN" = "pk.eyJ1IjoicnVnZWVyIiwiYSI6ImNqOTduN2phMTBmYXkyd29yNjR1amU2cjUifQ.IhNRZRmy1mlbLloz-p6vbw")
# Set the maximum file size to upload
options(shiny.maxRequestSize = 30 * 1024 ^ 3)

server <- function(input, output, session) {
  # Main object where most data is stored
  data <- reactiveValues(summary = NULL, object = NULL, selectedSessions = NULL, hasData = NULL)
  # Load named vectors
  choices <- trackeR:::choices()
  metrics <- trackeR:::metrics()
  ##################################################################################
  ## Home
  home <- paste0(normalizePath("~"), "/")
  ##################################################################################
  ## Upload and process data
  observeEvent(input$uploadButton, {
    if ((is.null(input$rawDataDirectory$datapath)) & (is.null(input$processedDataPath$datapath))) {
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
    } else {
      processed_data <- raw_data <- NULL
      ## if (processedDataPath() != home) {
      if (!is.null(input$processedDataPath$datapath)) {
        processed_data <- readRDS(input$processedDataPath$datapath)
      }
      if (!is.null(input$rawDataDirectory$datapath)) {
        file <- input$rawDataDirectory$datapath
        directory <- paste0(do.call(
          paste,
          c(
            as.list(c(Reduce(intersect, strsplit(file, "/")))),
            list(sep = "/")
          )
        ), "/")
        # Process raw data  
        raw_data <- callModule(
          module = trackeR:::readDirectory_shiny,
          id = "datafile",
          directory = directory,
          timezone = "GMT", 
          parallel = TRUE,
          cores = getOption("mc.cores", 4L),
          correctDistances = FALSE
        )
      }
      ## Remove duplicate sessions and create trackeRdata object from both raw and processed data
      data$object <- sort(unique(trackeR:::c.trackeRdata(processed_data, raw_data)), decreasing = FALSE)
      
      ## Generate a summary object, change units, set thresholds, check which sessions have data
      trackeR:::process_dataset(data)
      ## Update sport attribute of data$object with classified sports
      trackeR:::classify_sessions_by_sport(data)

      output$download_data <- trackeR:::download_handler(data)
      shinyjs::disable(selector = "#uploadButton")
      data$selectedSessions <- data$summary$session
      data$sessions_map <- rep(data$summary$session, times=1, each=2)
      shinyjs::click("plotButton")
      trackeR:::update_metrics_to_plot_workouts(session, choices, data$hasData)
    }
  })

  observeEvent(input$uploadSampleDataset, {
    removeModal()
    ## data(runs)
    ## data$object <- runs
    data(runs) # saved as df
    
    data$object <- runs
    ## See helper functions file
    trackeR:::process_dataset(data)
    ## See helper functions file
    trackeR:::classify_sessions_by_sport(data)
    output$download_data <- trackeR:::download_handler(data)
    shinyjs::disable(selector = "#uploadButton")
    data$selectedSessions <- data$summary$session
    shinyjs::click("plotButton")
    trackeR:::update_metrics_to_plot_workouts(session, choices, data$hasData)
  })

  ##################################################################################
  ## Change units
  observeEvent(input$showModalUnits, {
    if (!is.null(data$object)) {
      trackeR:::show_change_unit_window(data)
    } else {
      trackeR:::show_warning_window()
    }
  })
  observeEvent(input$updateUnits, {
    data$object <- trackeR:::change_units(data, input, "object")
    data$summary <- trackeR:::change_units(data, input, "summary")
    removeModal()
  })
  ##################################################################################
  ## Main page
  observeEvent({
    input$plotButton
  }, {
    if (is.null(data$object)) {
      trackeR:::show_warning_window()
    }
    else {
      output$timeline_plot <- plotly::renderPlotly({
        if (!is.null(data$summary)) {
          trackeR:::plot_timeline(data$summary, session = data$selectedSessions)
        }
      })
      ## Re-render all plots
      removeUI(selector = ".main_plots", immediate = TRUE, multiple = TRUE)
      sports_options <- c(
        "Running" = "running",
        "Cycling" = "cycling",
        "Swimming" = "swimming"
      )
      trackeR:::create_option_box(sport_options = sports_options[sapply(sports_options, function(x) {
        x %in% unique(sport(data$object))
      })])

      trackeR:::create_summary_timeline_boxes()
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
      ## DT
      output$summary <- trackeR:::render_summary_table(data, input)
      trackeR:::create_map()
      # output$map <- leaflet::renderLeaflet({
      #                            trackeR:::plot_map(x = data$object, session = data$selectedSessions, sumX = data$summary)
      #                        })
      preped_route_map <- reactive({
        session <- seq_along(data$object)
        prepRoute(data$object, session = session, threshold = TRUE)
      })
      output$map <- plotly::renderPlotly({
        trackeR:::plot_map(x = data$object, preped_route = preped_route_map(), 
                           session = isolate(data$selectedSessions), sumX = data$summary)
        
      })
      # Update map based on current selection
      observeEvent(data$selectedSessions, {
        plot_df <- preped_route_map()[which(preped_route_map()$SessionID %in% data$selectedSessions), ]
        #
        # plotlyProxy("map", session) %>%
        #   plotlyProxyInvoke("addTraces", list(lon = as.vector(plot_df$longitude),
        #                                       lat = as.vector(plot_df$latitude),
        #                                       type = 'scattermapbox',
        #                                       mode = 'lines'))
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
          list(mapbox.zoom = 5)
        )
        plotly::plotlyProxy("map", session) %>% plotly::plotlyProxyInvoke(
          "relayout",
          list(mapbox.center = list(
            lat = median(plot_df$latitude),
            lon = median(plot_df$longitude)
          ))
        )
      })
      trackeR:::create_summary_boxes()
      output$avgDistance_box <- trackeR:::render_summary_box("distance", "Average distance", data)
      output$avgDuration_box <- trackeR:::render_summary_box("duration", "Average duration", data)
      output$avgHeartRate_box <- trackeR:::render_summary_box("avgHeartRate", "Average heart rate", data)
      output$avgPace_box <- trackeR:::render_summary_box("avgPace", "Average pace", data)

      for (i in input$metricsSelected) {
        trackeR:::create_workout_plots(i)
      }
      lapply(input$metricsSelected, function(i) {
        output[[paste0(i, "_plot")]] <- plotly::renderPlotly({
          if (!is.null(input$sports)) {
            selected_sports <- data$summary$session[sport(data$object) %in% input$sports]
          } else {
            selected_sports <- data$summary$session
          }

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
    have_data_metrics_selected <- reactive({
      !sapply(metrics, function(metric) {
        all(sapply(data$object[data$selectedSessions], function(x) all((is.na(x[, metric])) | (x[, metric] == 0))))
      })
    })


    for (i in c(metrics[have_data_metrics_selected()])) {
      trackeR:::create_selected_workout_plot(id = i, collapsed = if (i %in% c("speed", "heart.rate", "altitude")) FALSE else TRUE)
    }
    cycling <- reactive({
      units <- getUnits(data$object)
      units$unit[units$variable == "cadence"] == "rev_per_min"
    })
    is_work_capacity <- reactive({
      if ((cycling()) & (all(sapply(data$object[data$selectedSessions], function(x) all((is.na(x[, "power"])) | (x[, "power"] == 0)))))) {
        NULL
      } else {
        "work_capacity"
      }
    })
    if (!is.null(is_work_capacity())) {
      # Need to change 'cycling' once attribute fixed
      trackeR:::create_work_capacity_plot(id = "work_capacity", collapsed = TRUE, cycling = cycling())
    }

    lapply(c(metrics[have_data_metrics_selected()], is_work_capacity()), function(i) {
      ## Render UI for time in zones plot
      output[[paste0(i, "_plot")]] <- renderUI({
        shinycssloaders::withSpinner(plotly::plotlyOutput(paste0(i, "Plot"), width = if (length(data$selectedSessions) > 2) {
          paste0(toString(500 * length(as.vector(data$selectedSessions))), "px")
        } else {
          "auto"
        }, height = "250px"), size = 2)
      })

      if (i != "work_capacity") {
        output[[paste0(i, "Plot")]] <- plotly::renderPlotly({
          if (!is.null(input[[paste0("detect_changepoints", i)]])) {
            fit_changepoint <- input[[paste0("detect_changepoints", i)]] > 0
          }
          trackeR:::plot_selectedWorkouts(
            x = data$object, session = data$selectedSessions, what = i, sumX = data$summary,
            changepoints = fit_changepoint,
            n_changepoints = as.numeric(input[[paste0("n_changepoints", i)]])
          )
          ## as.numeric(input[[paste0('n_changepoints', i)]]
        })
      } else {
        output$work_capacityPlot <- plotly::renderPlotly({
          label <- if (unique(sport(data$object[data$selectedSessions])) == 'cycling'){
            "Critical power [J]" } else {"Critical speed [m/s]"}
          updateNumericInput(session, inputId = "critical_power", label = label)
          change_power$value
          trackeR:::plot_work_capacity(
            x = data$object, session = data$selectedSessions,
            cp = isolate(as.numeric(input$critical_power))
          )
        })
      }
    })
    change_power <- reactiveValues(value=0)
    observeEvent(input$update_power, {
      # When the button is clicked, wrap the code in a call to `withBusyIndicatorServer()`
      withBusyIndicatorServer("update_power", {
        Sys.sleep(1)
        if (!is.numeric(input$critical_power) | input$critical_power <= 0) {
          stop("Invalid input. Input has to be a positive numeric value.")
        } else {
          change_power$value <- change_power$value + 1
        }
      })
    })

    trackeR:::create_zones_box(
      title = "Time in Zones", inputId = "zonesMetricsPlot",
      label = "Select zone metrics to plot", plotId = "zonesPlotUi",
      choices = metrics[have_data_metrics_selected()]
    )
    trackeR:::create_profiles_box(
      title = "Concentration profiles", inputId = "profileMetricsPlot",
      label = "Select profile metrics to plot", plotId = "concentration_profiles",
      choices = metrics[have_data_metrics_selected()]
    )

    ## Render UI for time in zones plot
    output$zonesPlotUi <- renderUI({
      shiny::req(input$zonesMetricsPlot)
      shinycssloaders::withSpinner(plotly::plotlyOutput(
        "zones_plot", width = "100%",
        height = trackeR:::calculate_plot_height(input$zonesMetricsPlot)
      ), size = 2)
    })
    ## Render actual plot
    output$zones_plot <- plotly::renderPlotly({
      trackeR:::plot_zones(
        x = data$object, session = data$selectedSessions, what = input$zonesMetricsPlot,
        n_zones = as.numeric(input$n_zones)
      )
    })

    ## Render UI for concentration profiles
    output$concentration_profiles <- renderUI({
      shiny::req(input$profileMetricsPlot)
      shinycssloaders::withSpinner(plotly::plotlyOutput(
        "conc_profiles_plots", width = "auto",
        height = trackeR:::calculate_plot_height(input$profileMetricsPlot)
      ), size = 2)
    })

    ## Render actual plot
    output$conc_profiles_plots <- plotly::renderPlotly({
      trackeR:::plot_concentration_profiles(x = data$object, session = data$selectedSessions, 
                                            what = input$profileMetricsPlot)
    })
  }, once = TRUE)

  ##################################################################################
  observeEvent(input$return_to_main_page, {
    shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    output$cond <- reactive({
      TRUE
    })
  })
  observeEvent(input$plotSelectedWorkouts, {
    shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    output$cond <- reactive({
      FALSE
    })
  })
  ##################################################################################
}