#   ____________________________________________________________________________
#   Live version configuration                                              ####
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
options(shiny.maxRequestSize = 30 * 1024^3)

#   ____________________________________________________________________________
#   Server                                                                  ####
server <- function(input, output, session) {
  # Main object where most data is stored
  data <- reactiveValues(
    summary = NULL, object = NULL,
    selectedSessions = NULL, hasData = NULL
  )
  # Store the pervious value to let user upload new data constantly
  previous_file_paths <- reactiveValues(processed = 'NULL')

  # Load named vectors
  choices <- trackeR:::choices()
  metrics <- trackeR:::metrics()
##  ............................................................................
##  Upload data                                                             ####
  observeEvent(input$uploadButton, {
    no_raw_directory_selected <- is.null(input$rawDataDirectory$datapath)
    no_processed_file_selected <- is.null(input$processedDataPath$datapath)
    if (no_raw_directory_selected & no_processed_file_selected) {
      trackeR:::show_warning_no_data_selected()
    } else {
      
      processed_data <- raw_data <- NULL
      if (!no_processed_file_selected) {
        if (input$processedDataPath$datapath != previous_file_paths$processed) {
        # Load processed data
        processed_data <- readRDS(input$processedDataPath$datapath)
        }
      }
      if (!no_raw_directory_selected) {
        file <- input$rawDataDirectory$datapath
        # Get directory path
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
          correctDistances = FALSE
        )
      }
      previous_file_paths$processed <- input$processedDataPath$datapath
### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Process uploaded data                                                   ####
      ## Remove duplicate sessions and create trackeRdata object from both raw and processed data
      data$object <- sort(unique(trackeR:::c.trackeRdata(processed_data, raw_data,
                                                         data$object)), decreasing = FALSE)
      ## See helper file
      trackeR:::generate_objects(data, output, session, choices)
    }
  })

##  ............................................................................
##  Uploading sample dataset                                                ####
  observeEvent(input$uploadSampleDataset, {
    removeModal()
    data(runs) # NOTE: check if optimal way to use dataset from the same package
    data$object <- runs
    # See helper file
    trackeR:::generate_objects(data, output, session, choices)
  })

##  ............................................................................
##  Change units                                                            ####
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

#   ____________________________________________________________________________
#   Session summaries page                                                  ####
  observeEvent({
    input$plotButton
  }, {
    if (is.null(data$object)) {
      trackeR:::show_warning_window()
    } else {
      output$timeline_plot <- plotly::renderPlotly({
        if (!is.null(data$summary)) {
          trackeR:::plot_timeline(data$summary, session = data$selectedSessions)
        }
      })
      # Re-render all plots
      removeUI(selector = ".main_plots", immediate = TRUE, multiple = TRUE)
      sports_options <- trackeR:::sports_options
      identified_sports <- sports_options %in% unique(trackeR::sport(data$object))
      trackeR:::create_option_box(sports_options[identified_sports])
### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Summary table                                                           ####
      trackeR:::create_summary_timeline_boxes()
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
      output$summary <- trackeR:::render_summary_table(data, input)
### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Map                                                                     ####
      trackeR:::create_map()
      preped_route_map <- reactive({
        session <- seq_along(data$object)
        prepRoute(data$object, session = session, threshold = TRUE)
      })
      output$map <- plotly::renderPlotly({
        trackeR:::plot_map(
          x = data$object, preped_route = preped_route_map(),
          session = isolate(data$selectedSessions), sumX = data$summary
        )
      })
      # Update map based on current selection
      observeEvent(data$selectedSessions, {
        sessions_rows <- which(preped_route_map()$SessionID %in% data$selectedSessions)
        plot_df <- preped_route_map()[sessions_rows, ]
        trackeR:::update_map(plot_df, session, data)
      })
### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Summary boxes                                                           ####
      trackeR:::create_summary_boxes()
      output$avgDistance_box <- trackeR:::render_summary_box("distance", "Average distance", data)
      output$avgDuration_box <- trackeR:::render_summary_box("duration", "Average duration", data)
      output$avgHeartRate_box <- trackeR:::render_summary_box("avgHeartRate", "Average heart rate", data)
      output$avgPace_box <- trackeR:::render_summary_box("avgPace", "Average pace", data)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Sessions summaries plots                                                ####
      for (i in input$metricsSelected) {
        trackeR:::create_workout_plots(i)
      }
      lapply(input$metricsSelected, function(i) {
        output[[paste0(i, "_plot")]] <- plotly::renderPlotly({
          if (!is.null(input$sports)) {
            selected_sessions_by_sport <- sport(data$object) %in% input$sports
            selected_sports <- data$summary$session[selected_sessions_by_sport]
          } else {
            selected_sports <- data$summary$session
          }
          trackeR:::plot_workouts(sumX = data$summary, what = i)
        })
      })
      # Set to TRUE such that all plots are visible
      output$cond <- reactive({
        TRUE
      })
      outputOptions(output, "cond", suspendWhenHidden = FALSE)
    }
  })

#   ____________________________________________________________________________
#   Individual sessions page                                                ####
  observeEvent(input$plotSelectedWorkouts, {
    shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    # Test which metrics have data for selected sessions
    have_data_metrics_selected <- reactive({
      !sapply(metrics, function(metric) {
        all(sapply(data$object[data$selectedSessions], {
          function(x) all((is.na(x[, metric])) | (x[, metric] == 0))
        }))
      })
    })

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Generate individual sessions plots (except work capacity)               ####
    metrics_to_collapse <- c("speed", "heart.rate", "altitude")
    for (i in c(metrics[have_data_metrics_selected()])) {
      collapse <- if (i %in% metrics_to_collapse) FALSE else TRUE
      trackeR:::create_selected_workout_plot(id = i, collapsed = collapse)
    }
    
    lapply(metrics[have_data_metrics_selected()], function(i) {
      plot_width <- reactive({if (length(data$selectedSessions) > 2) {
        paste0(toString(500 * length(as.vector(data$selectedSessions))), "px")
      } else {
        "auto"
      }})
      output[[paste0(i, "_plot")]] <- renderUI({
        shinycssloaders::withSpinner(plotly::plotlyOutput(paste0(i, "Plot"),
          width = plot_width(),
          height = "250px"
        ),
        size = 2
        )
      })

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Render individual sessions plots (except work capacity)                 ####
      output[[paste0(i, "Plot")]] <- plotly::renderPlotly({
        # Whether to detect changepoints
        if (!is.null(input[[paste0("detect_changepoints", i)]])) {
          fit_changepoint <- input[[paste0("detect_changepoints", i)]] > 0
        }
        trackeR:::plot_selectedWorkouts(
          x = data$object, session = data$selectedSessions, what = i,
          sumX = data$summary, changepoints = fit_changepoint,
          n_changepoints = as.numeric(input[[paste0("n_changepoints", i)]])
        )
      })
    })

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Generate work capacity plot                                             ####
    # Check which work capacity plots to generate
    work_capacity_ids <- reactive({
      trackeR:::test_work_capacity(data)
    })

    trackeR:::create_work_capacity_plot(id = 'work_capacity')
    
    sapply(c('cycling', 'running'), function(sport_id) {
      output[[paste0(sport_id, "_work_capacity_plot")]] <- renderUI({
        n_sessions <- sum(trackeR::sport(data$summary[data$selectedSessions]) %in% sport_id)
        plot_width <- if (n_sessions > 2) {
          paste0(toString(500 * n_sessions), "px")
        } else {
          "auto"
        }
        shinycssloaders::withSpinner(plotly::plotlyOutput(paste0(sport_id, "Plot"),
                                                          width = plot_width,
                                                          height = "250px" ), size = 2
        )
      }) 
    
### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Render work capacity                                                    ####
    output[[paste0(sport_id, "Plot")]] <- plotly::renderPlotly({
      # TODO automatically update units 
      # label <- if (sport == "cycling") {
      #   "Critical power [J]"
      # } else {
      #   "Critical speed [m/s]"
      # }
      # updateNumericInput(session, inputId = "critical_power", label = label)
      
      # If button to change units is pressed re-render plot with new units
      change_power[[sport_id]]

      work_capacity_sessions <- trackeR::sport(data$summary[data$selectedSessions]) %in% sport_id
      trackeR:::plot_work_capacity(
        x = data$object, session = data$selectedSessions[work_capacity_sessions],
        cp = isolate(as.numeric(input[[paste0('critical_power_', sport_id)]]))
      )
    })
  })

  # Conditions for displaying the work capacity plot
  output[[paste0('work_capacity_running')]] <- reactive({
    if ('running' %in%  work_capacity_ids()) {
    FALSE
    } else {
      TRUE
    }
  })
  
  output[[paste0('work_capacity_cycling')]] <- reactive({
    if ('cycling' %in%  work_capacity_ids()) {
      FALSE
    } else {
      TRUE
    }
  })
  outputOptions(output, 'work_capacity_cycling', suspendWhenHidden = FALSE)    
  outputOptions(output, 'work_capacity_running', suspendWhenHidden = FALSE)
### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Update power for work capacity plot                                     ####
    change_power <- reactiveValues(cycling = 0, running = 0)
    observeEvent(input$cycling_update_power, { 
      trackeR:::withBusyIndicatorServer("cycling_update_power", {
        Sys.sleep(1)
        if (!is.numeric(input$critical_power_cycling) | input$critical_power_cycling <= 0) {
          stop("Invalid input. Input has to be a positive numeric value.")
        } else {
          change_power$cycling <- change_power$cycling + 1
        }
      })
    })
    
    observeEvent(input$running_update_power, {
      trackeR:::withBusyIndicatorServer("running_update_power", {
        Sys.sleep(1)
        if (!is.numeric(input$critical_power_running) | input$critical_power_running <= 0) {
          stop("Invalid input. Input has to be a positive numeric value.")
        } else {
          change_power$running <- change_power$running + 1
        }
      })
    })

##  ............................................................................
##  Time in zones                                                           ####
    trackeR:::create_zones_box(
      title = "Time in Zones", inputId = "zonesMetricsPlot",
      label = "Select zone metrics to plot", plotId = "zonesPlotUi",
      choices = metrics[have_data_metrics_selected()]
    )
    ## Render UI for time in zones plot
    output$zonesPlotUi <- renderUI({
      shiny::req(input$zonesMetricsPlot)
      shinycssloaders::withSpinner(plotly::plotlyOutput(
        "zones_plot",
        width = "100%",
        height = trackeR:::calculate_plot_height(input$zonesMetricsPlot)
      ), size = 2)
    })
    ## Render actual plot
    output$zones_plot <- plotly::renderPlotly({
      trackeR:::plot_zones(
        x = data$object, session = data$selectedSessions,
        what = input$zonesMetricsPlot,
        n_zones = as.numeric(input$n_zones)
      )
    })

##  ............................................................................
##  Concentration profiles                                                  ####
    trackeR:::create_profiles_box(
      title = "Concentration profiles", inputId = "profileMetricsPlot",
      label = "Select profile metrics to plot", plotId = "concentration_profiles",
      choices = metrics[have_data_metrics_selected()]
    )
    ## Render UI for concentration profiles
    output$concentration_profiles <- renderUI({
      shiny::req(input$profileMetricsPlot)
      shinycssloaders::withSpinner(plotly::plotlyOutput(
        "conc_profiles_plots",
        width = "auto",
        height = trackeR:::calculate_plot_height(input$profileMetricsPlot)
      ), size = 2)
    })
    ## Render actual plot
    output$conc_profiles_plots <- plotly::renderPlotly({
      trackeR:::plot_concentration_profiles(
        x = data$object,
        session = data$selectedSessions,
        what = input$profileMetricsPlot
      )
    })
  }, once = TRUE)


#   ____________________________________________________________________________
#   Toggle between session summaries page and individual sessions page      ####
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
##  ............................................................................
##  Reset button                                                            ####
  observeEvent(input$resetButton, {
    shinyjs::js$reset_page()
  })
}

