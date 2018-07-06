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
  # Ensure that when button for changepoints clicked, the window does not 
  # dissapear by a click in the window.
  shinyjs::runjs(
    "$(document).on('click', '.dropdown-menu', function (e) {
    e.stopPropagation();
  });"
  )
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
        raw_data <- trackeR:::read_directory_shiny(
          directory = directory,
          timezone = "GMT",
          parallel = TRUE,
          correct_distances = FALSE
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
      data$limits <- trackeR::compute_limits(data$object, a = 0.05)
      data$no_location_data <- sapply(data$object, 
          function(x) all((is.na(x[, 'longitude'])) | (x[, 'longitude'] == 0))
        )
    }  
  })
### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Selected sessions                                                       ####
proxy <- DT::dataTableProxy('summary')
# Sessions selected from plots using box/lasso selection
observeEvent(plotly::event_data("plotly_selected"), {
  trackeR:::generate_selected_sessions_object(data, input,
    plot_selection = TRUE
  )
  if (length(data$selectedSessions) != length(data$summary$session)) {
    DT::selectRows(proxy = proxy, selected = as.numeric(data$selectedSessions))
  } else {
    DT::selectRows(proxy = proxy, selected = NULL)
  }
  # trackeR:::update_sport_selection(data, session)
})

# Sessions selected by sport using radio buttons
observeEvent(input$sports, {
  shinyjs::js$resetSelection()
  trackeR:::generate_selected_sessions_object(data, input, sport_selection = TRUE)
  
  if (length(data$selectedSessions) != length(data$summary$session)) {
    DT::selectRows(proxy = proxy, selected = as.numeric(data$selectedSessions))
  } else {
    DT::selectRows(proxy = proxy, selected = NULL)
  }
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  ### Update metrics available based on sport selected                        ####
  has_data_sport <- lapply(data$summary[which(trackeR::get_sport(data$summary) %in% input$sports)], function(session_summaries) {
    !all(is.na(session_summaries) | session_summaries == 0)
  })
  selected_metrics <- c(input$metricsSelected[sapply(input$metricsSelected, function(x) {
    has_data_sport[[x]]
  })])
  metrics_available_sport <- reactive({c(choices[sapply(choices, function(x) {
    has_data_sport[[x]]
  })])
  })
  shinyWidgets::updatePickerInput(session = session, inputId = 'metricsSelected', 
                                  selected = selected_metrics, 
                                  choices = metrics_available_sport())
}, ignoreNULL = FALSE, ignoreInit = TRUE)

# Sessions selected through summary table
observeEvent(input$summary_rows_selected,  {
    # trackeR:::update_sport_selection(data, session)
    shinyjs::js$resetSelection()
    trackeR:::generate_selected_sessions_object(data, input, 
                                                table_selection = TRUE)
}, ignoreNULL = TRUE)

# Reset button clicked
observeEvent(input$resetSelection, {
  trackeR:::update_sport_selection(data, session)
  shinyjs::js$resetSelection()
  DT::selectRows(proxy = proxy, selected = NULL)
  trackeR:::generate_selected_sessions_object(data, input, no_selection = TRUE)
})

##  ............................................................................
##  Uploading sample dataset                                                ####
  observeEvent(input$uploadSampleDataset, {
    removeModal()
    filepath <- system.file('inst/extdata/sample.rds', package = 'trackeR')
    data$object <- readRDS(filepath)
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
    data$object <- trackeR:::change_object_units(data, input, "object")
    data$summary <- trackeR:::change_object_units(data, input, "summary")
    removeModal()
  })

#   ____________________________________________________________________________
#   Session summaries page                                                  ####
  observeEvent({
    input$createDashboard
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
      # removeUI(selector = ".main_plots", immediate = TRUE, multiple = TRUE)
      sports_options <- trackeR:::sports_options
      identified_sports <- sports_options %in% unique(trackeR::get_sport(data$object))
      metrics_available <- reactive({c(choices[sapply(choices, function(x) {
        data$hasData[[x]]
      })])
      })
      trackeR:::create_option_box(sport_options = sports_options[identified_sports],
                                  metrics_available = metrics_available())


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Summary table                                                           ####
      trackeR:::create_summary_timeline_boxes()
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
      output$summary <- trackeR:::render_summary_table(data, input)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Summary boxes                                                           ####
trackeR:::create_summary_boxes()
output$avgDistance_box <- trackeR:::render_summary_box("distance",
                                                       "Average distance", data)
output$avgDuration_box <- trackeR:::render_summary_box("duration", 
                                                       "Average duration", data)
output$avgHeartRate_box <- trackeR:::render_summary_box("avgHeartRate",
                                                        "Average heart rate", data)
output$avgPace_box <- trackeR:::render_summary_box("avgPace", 
                                                   "Average pace", data)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Map                                                                     ####
      # Check if there is internet connection
      test_connection <- try(RCurl::getURL("www.google.com"), silent = TRUE)
      if (class(test_connection) == "try-error") {
        is_internet_connection <- FALSE
      } else {
        is_internet_connection <- TRUE
      }
      # do not generate map if no location data for at least one session
      # TODO allow to plot only sessions that do have location data
      if ((!any(data$no_location_data)) & (is_internet_connection)) {
        trackeR:::create_map()
        
        preped_route_map <- reactive({
          session <- seq_along(data$object)
          prepare_route(data$object,
                    session = session, threshold = TRUE)
        })
        output$map <- plotly::renderPlotly({
          trackeR:::plot_map(
            x = data$object,
            preped_route = preped_route_map(),
            session = isolate({data$selectedSessions}),
            sumX = data$summary
          )
        })
        # Update map based on current selection
        observeEvent(data$selectedSessions, {
          sessions_rows <- which(preped_route_map()$SessionID %in% data$selectedSessions)
          plot_df <- preped_route_map()[sessions_rows, ]
          trackeR:::update_map(plot_df, session, data)
        })
      }

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Sessions summaries plots                                                ####
      # Generate conditional plot for each metric irrespective of whether data available
      for (metric in c(choices)) {
        trackeR:::create_workout_plots(metric)
      }
      sapply(c(choices), function(i) {
        output[[paste0(i, "_plot")]] <- plotly::renderPlotly({
          sessions_to_plot <- if (is.null(input$sports)) {
            data$summary$session
            } else {
            data$summary$session[get_sport(data$object) %in% input$sports] 
            }
          trackeR:::plot_workouts(sumX = data$summary[sessions_to_plot],
                                  what = i, 
                                  sessions = data$selectedSessions,
                                  sports = trackeR::get_sport(data$object)[sessions_to_plot])
        })
      })
      # Set to TRUE such that all plots are visible
      output$cond <- reactive({
        TRUE
      })
      outputOptions(output, "cond", suspendWhenHidden = FALSE)
      data$show_summary_plots <- TRUE
      sapply(c(choices), function(choice) {
        output[[choice]] <- reactive({
          if ((choice %in% input$metricsSelected) & (data$show_summary_plots)) {
            FALSE
          } else {
            TRUE
          }
        })
        outputOptions(output, choice, suspendWhenHidden = FALSE)
      })

    }
  }, once = TRUE)

  
### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Test which metrics have data                                            ####
  have_data_metrics_selected <- reactive({
    !sapply(metrics, function(metric) {
      all(sapply(data$object[data$selectedSessions], {
        function(x) all((is.na(x[, metric])) | (x[, metric] == 0))
      }))
    })
  })
#   ____________________________________________________________________________
#   Individual sessions page                                                ####
  observeEvent(input$plotSelectedWorkouts, {
    shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    ##  ............................................................................
    ##  Time in zones                                                           ####
    trackeR:::create_zones_box(
      inputId = "zonesMetricsPlot",
      plotId = "zonesPlotUi",
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
    # Update metrics available each time different sessions selected
    observeEvent(data$selectedSessions, {
      # shiny::updateSelectizeInput(session = session, inputId = "zonesMetricsPlot", 
      #                             choices =  metrics[have_data_metrics_selected()], 
      #                             selected = 'speed'
      # )
      shinyWidgets::updatePickerInput(session = session, inputId = "zonesMetricsPlot", 
                                      choices =  metrics[have_data_metrics_selected()], 
                                      selected = 'speed')
    }, ignoreInit = TRUE)
    
### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Generate individual sessions plots (except work capacity)               ####
    # metrics_to_expand <- c("speed", "heart_rate", "altitude")
    metrics_to_expand <- c('speed')
    # First generate all plots irrespective if data available
    for (i in c(metrics)) {
      collapse <- if (i %in% metrics_to_expand) FALSE else TRUE
      i <- if (i == 'heart_rate') "heart_rate" else i
      trackeR:::create_selected_workout_plot(id = i, collapsed = collapse)
    }
    
    sapply(metrics, function(i) {
      plot_width <- reactive({if (length(data$selectedSessions) > 3) {
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
          threshold = FALSE,
          n_changepoints = isolate(as.numeric(input[[paste0("n_changepoints", i)]])),
          desampling = 1
        )
      })
      
      output[[i]] <- reactive({
        if ((i %in% metrics[have_data_metrics_selected()]) & data$show_individual_sessions) {
          FALSE
        } else {
          TRUE
        }
      })
      outputOptions(output, i, suspendWhenHidden = FALSE)  
    })
    
    # sapply(metrics, function(metric) {
    #   # Conditions for displaying the work capacity plot
    #   output[[paste0(metric, '_123')]] <- reactive({
    #     TRUE
    #   })
    #   # outputOptions(output, paste0(metric, '_123'), suspendWhenHidden = FALSE)  
    # })

##  ............................................................................
##  Concentration profiles                                                  ####
    trackeR:::create_profiles_box(
      inputId = "profileMetricsPlot",
      plotId = "concentration_profiles",
      choices = metrics[have_data_metrics_selected()],
      collapsed = TRUE
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
    concentration_profiles <- reactive({
      trackeR::concentration_profile(data$object, 
                            what = metrics[have_data_metrics_selected()],
                            limits = data$limits)
    })
    ## Render actual plot
    output$conc_profiles_plots <- plotly::renderPlotly({
      trackeR:::plot_concentration_profiles(
        x = data$object,
        session = data$selectedSessions,
        what = input$profileMetricsPlot, 
        profiles_calculated = concentration_profiles()
      )
    })
    
    # Update metrics available each time different sessions selected
    observeEvent(data$selectedSessions, {
      # shiny::updateSelectizeInput(session = session, inputId = "profileMetricsPlot", 
      #                             choices = metrics[have_data_metrics_selected()],
      #                             selected = 'speed'
      # )
      shinyWidgets::updatePickerInput(session = session, inputId = "profileMetricsPlot", 
                                      choices =  metrics[have_data_metrics_selected()], 
                                      selected = 'speed')
    }, ignoreInit = TRUE)
    ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
    ### Generate work capacity plot                                             ####
    # Check which work capacity plots to generate
    work_capacity_ids <- reactive({
      trackeR:::test_work_capacity(data)
    })
    
    trackeR:::create_work_capacity_plot(id = 'work_capacity')
    
    sapply(c('cycling', 'running'), function(sport_id) {
      output[[paste0(sport_id, "_work_capacity_plot")]] <- renderUI({
        n_sessions <- sum(trackeR::get_sport(data$summary[data$selectedSessions]) %in% sport_id)
        plot_width <- if (n_sessions > 3) {
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
        
        work_capacity_sessions <- trackeR::get_sport(data$summary[data$selectedSessions]) %in% sport_id
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
    
    output[['work_capacity']] <- reactive({
      if ((length(work_capacity_ids()) != 0) & data$show_work_capacity) {
        FALSE
      } else {
        TRUE
      }
    })
    outputOptions(output, 'work_capacity_cycling', suspendWhenHidden = FALSE)    
    outputOptions(output, 'work_capacity_running', suspendWhenHidden = FALSE)
    outputOptions(output, 'work_capacity', suspendWhenHidden = FALSE)   
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
    
  }, once = TRUE)
  
#   ____________________________________________________________________________
#   Toggle between session summaries page and individual sessions page      ####
  observeEvent(input$return_to_main_page, {
    shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    output$cond <- reactive({
      TRUE
    })
    data$show_summary_plots <- TRUE
    data$show_individual_sessions <- FALSE
    data$show_work_capacity <- FALSE
  })
  observeEvent(input$plotSelectedWorkouts, {
    shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    output$cond <- reactive({
      FALSE
    })
    data$show_summary_plots <- FALSE
    data$show_individual_sessions <- TRUE
    data$show_work_capacity <- TRUE
  })
##  ............................................................................
##  Reset button                                                            ####
  observeEvent(input$resetButton, {
    shinyjs::js$reset_page()
  })
}

