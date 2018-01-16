server <- function(input, output, session) {

    ## ## Metrics for cycling
    ## metrics <- c('Distance' = 'distance',
    ##              'Duration' = 'duration',
    ##              'Average speed' = 'avgSpeed',
    ##              'Average pace' = 'avgPace',
    ##              'Average cadence' = 'avgCadence',
    ##              'Average heart rate' = 'avgHeartRate'
    ##              )
    ## observeEvent(input$sportSelected, { if(input$sportSelected == 'running') {
    ##                                         updateSelectizeInput(session, 'metricsSelected', choices = metrics, server = TRUE)
    ##                                     }
    ## })
    ## Disable selection of Power if sport is running
    ## storing all data
    data <- reactiveValues()

    ## directory
    shinyDirChoose(input, 'raw_data_directory', roots = c(home = '~'),
                   filetypes = c('gpx', "tcx", "db3", "json"))
    raw_data_directory <- reactive(input$raw_data_directory)
    raw_data_path <- reactive({
        home <- normalizePath("~")
        file.path(home, paste(unlist(raw_data_directory()$path[-1]),
                              collapse = .Platform$file.sep))
    })

    ## Processed data
    shinyFileChoose(input, 'processed_data_path', roots = c(home = '~'),
                    filetypes = c('rds', 'rdata'))
    processed_data_file <- reactive(input$processed_data_path)
    processed_data_path <- reactive({
        home <- normalizePath("~")
        file.path(home, paste(unlist(processed_data_file()$files)[-1],
                              collapse = .Platform$file.sep))
    })

    ## Home
    home <- paste0(normalizePath("~"), '/')

    ## Units
    observeEvent(input$changeUnits, {
        req(data$trackeRdata_object)
        get_selected_units <- function(feature){
            getUnits(data$trackeRdata_object)$unit[getUnits(data$trackeRdata_object)$variable %in% feature]
        }
        showModal(modalDialog(
            title = "Change units",
            radioButtons('altitudeUnits', 'Altitude:',
                         c('[m]' = 'm',
                           '[km]' = 'km',
                           '[mi]' = 'mi',
                           '[ft]' = 'ft'), inline = TRUE,
                         selected = get_selected_units('altitude')),
            radioButtons('distanceUnits', 'Distance:',
                         c('[m]' = 'm',
                           '[km]' = 'km',
                           '[mi]' = 'mi',
                           '[ft]' = 'ft'), inline = TRUE,
                         selected = get_selected_units('distance')),
            radioButtons('speedUnits', 'Speed:',
                         c('[m/s]' = 'm_per_s',
                           '[km/h]' = 'km_per_h',
                           '[ft/min]' = 'ft_per_min',
                           '[ft/s]' = 'ft_per_s',
                           '[mi/h]' = 'mi_per_h'), inline = TRUE,
                         selected = get_selected_units('speed')),
            radioButtons('cadenceUnits', 'Cadence:',
                         c('[steps/min]' = 'steps_per_min',
                           '[revolutions/min]' = 'rev_per_min'), inline = TRUE,
                         selected = get_selected_units('cadence')),
            radioButtons('powerUnits', 'Power:',
                         c('[W]' = 'W',
                           '[kW]' = 'kW'), inline = TRUE,
                         selected = get_selected_units('power')),
            radioButtons('paceUnits', 'Pace:',
                         c('[min/km]' = 'min_per_km',
                           '[min/mi]' = 'min_per_mi',
                           '[s/min]' = 's_per_m'), inline = TRUE,
                         selected = get_selected_units('pace')),
            footer = tagList(
                modalButton("Cancel"),
                actionButton("updateUnits", "Apply")
            )
        ))
    })

    observeEvent(input$updateUnits, {
        allUnits <- reactive({
            unused_variables <- c('latitude', 'longitude', 'heart.rate', 'duration')
            getUnits(data$trackeRdata_object)$variable[!(getUnits(data$trackeRdata_object)$variable %in% unused_variables)]
        })
        change_units <- function(data){
            units <- c()
            for (i in allUnits()){
                units <- c(units, input[[paste0(i, 'Units')]])
            }
            data_updated <- changeUnits(data, variable = allUnits(), unit = units)
            return(data_updated)
        }
        data$trackeRdata_object <- change_units(data$trackeRdata_object)
        data$summary <- change_units(data$summary)
        removeModal()
    })


    ## Upload
    observeEvent(input$uploadButton, {
        data$sport <- input$sportSelected == 'cycling'
        if ((raw_data_path() == home) & (processed_data_path() == home)) {
                showModal(modalDialog(title = 'trackeR dashboard message',
                                      div(tags$b("Please select a processed data image or a directory with raw datafiles",
                                                 class='warningMessage')),
                                      easyClose = TRUE,
                                      size = 's'))
        }
        else {
            processed_data <- raw_data <- NULL
            if (processed_data_path() != home) {
                processed_data <- readRDS(processed_data_path())
            }
            if (raw_data_path() != home) {
                raw_data <- callModule(module = readDirectory_reactive,
                                       id = "datafile",
                                       directory = raw_data_path(),
                                       timezone = "GMT", cycling = data$sport,
                                       correctDistances = TRUE)
            }
            data$trackeRdata_object <- c.trackeRdata(processed_data, raw_data)
            data$summary <- summary(data$trackeRdata_object)
            output$download_data <- downloadHandler(
                filename = function() {
                paste0("data-", Sys.Date(), ".RData")
            },
            content = function(file) {
                saveRDS(data$trackeRdata_object, file)
            }
            )
        }

        data$nsessions <- nsessions(data$summary)
        data$selected_sessions <- data$summary$session
    })

    observeEvent({
        input$updateUnits
        input$plotButton
    }, {
        shiny::req(data$trackeRdata_object)
        output$timeline_plot <- renderPlot({
            timeline(data$summary)
        })
        if (is.null(data$trackeRdata_object)) showModal(modalDialog(title = 'Important message',
                                                                    div(tags$b("Please click upload data",
                                                                               class='warningMessage')),
                                                                    easyClose = TRUE,
                                                                    size = 's'))
        removeUI(selector = ".plots", immediate = TRUE, multiple = TRUE)
        data$metrics <- input$metricsSelected
        data$button <- input$plotButton
        insertUI(
            selector = ".content",
            where = "beforeEnd",
            ui = conditionalPanel(condition = "output.cond == true", div(class='plots', fluidRow(box(
                                                                                            status = 'primary',
                                                                                            width = 12,
                                                                                            height = "500px",
                                                                                            title = tagList(shiny::icon("map"), 'Map'),
                                                                                            withSpinner(leafletOutput('map', width = "auto", height = "430px"), size = 2),
                                                                                            absolutePanel(top = 70, right = 60,
                                                                                                          sliderInput("average_speed", "Average speed",
                                                                                                                      floor(min(data$summary$avgSpeed[is.finite(data$summary$avgSpeed)], na.rm = TRUE)),
                                                                                                                      ceiling(max(data$summary$avgSpeed[is.finite(data$summary$avgSpeed)], na.rm=TRUE)),
                                                                                                                      value = c(floor(min(data$summary$avgSpeed[is.finite(data$summary$avgSpeed)], na.rm=TRUE)),
                                                                                                                                ceiling(max(data$summary$avgSpeed[is.finite(data$summary$avgSpeed)], na.rm=TRUE))),
                                                                                                                      step = 0.1, width = '200px'),
                                                                                                          sliderInput("average_heart_rate", "Average heart rate",
                                                                                                                      floor(min(data$summary$avgHeartRate[is.finite(data$summary$avgHeartRate)], na.rm=TRUE)/10)*10,
                                                                                                                      ceiling(max(data$summary$avgHeartRate[is.finite(data$summary$avgHeartRate)], na.rm=TRUE)/10)*10,
                                                                                                                      value = c(floor(min(data$summary$avgHeartRate[is.finite(data$summary$avgHeartRate)], na.rm=TRUE)/10)*10,
                                                                                                                                ceiling(max(data$summary$avgHeartRate[is.finite(data$summary$avgHeartRate)], na.rm=TRUE)/10)*10),
                                                                                                                      step = 1, width = '200px'),
                                                                                                          sliderInput("average_distance", "Distance",
                                                                                                                      floor(min(data$summary$distance[is.finite(data$summary$distance)], na.rm=TRUE)/1000)*1000,
                                                                                                                      ceiling(max(data$summary$distance[is.finite(data$summary$distance)], na.rm=TRUE)/1000)*1000,
                                                                                                                      value = c(floor(min(data$summary$distance[is.finite(data$summary$distance)], na.rm=TRUE)/1000)*1000,
                                                                                                                                ceiling(max(data$summary$distance[is.finite(data$summary$distance)], na.rm=TRUE)/1000)*1000),
                                                                                                                      step = 1, width = '200px'),
                                                                                                          actionButton('plotSelectedWorkouts', 'Plot selected workouts',
                                                                                                                       style="color: #fff; background-color: #428bca; border-color:#428bca")
                                                                                                          ))))))
        insertUI(
            selector = ".content",
            where = "beforeEnd",
            ui = conditionalPanel(condition = "output.cond == true",
                                  div(class='plots', fluidRow(
                                                         valueBoxOutput("averageDistance", width = 3),
                                                         valueBoxOutput("averageDuration", width = 3),
                                                         valueBoxOutput("averagePace", width = 3),
                                                         valueBoxOutput("averageHeartRate", width = 3)
                                                     ))))

        for (i in data$metrics){
            name <-reactive({switch(as.character(i),
                                    "distance" = "Distance",
                                    "duration" = "Duration",
                                    "avgSpeed" = "Average Speed",
                                    "avgPace" = "Average Pace",
                                    "avgCadence" = "Average Cadence",
                                    "avgPower" = "Average Power",
                                    "avgHeartRate" = "Average Heart Rate",
                                    "wrRatio" = "Work-to-rest Ratio")
            })
            insertUI(
                selector = ".content",
                where = "beforeEnd",
                ui = conditionalPanel(condition = "output.cond == true",
                                      div(class='plots', id = paste0("box", i), fluidRow(
                                                                                    box(
                                                                                        status = 'primary',
                                                                                        width = 12,
                                                                                        height = "250px",
                                                                                        title = tagList(shiny::icon(create_icon(i)), name()),
                                                                                        plotlyOutput(i, width = "auto", height = "180px")
                                                                                    )
                                                                                )))
            )
        }

        box_text <- function(what, subtitle, icon) {
            value <- reactive({
                value <- round(mean(data$summary[data$selected_sessions][[what]], na.rm = TRUE))
                if (is.na(value)) {
                    "not available"
                }
                else {
                    paste0(value, ' ', lab_sum(what, data$summary, FALSE))
                }
            })
            v <- value()
            valueBox(v, subtitle, icon, color = if (v == "not available") "olive" else "light-blue")
        }

        output$averageDistance <- renderValueBox({
            box_text("distance",
                     subtitle = "Average distance",
                     icon = icon(create_icon("distance")))
        })

        output$averageDuration <- renderValueBox({
            box_text("duration", "Average duration",
                     icon = icon(create_icon('duration')))
        })

        output$averageHeartRate <- renderValueBox({
            box_text("avgHeartRate", "Average heart rate",
                     icon = icon(create_icon('avgHeartRate')))
        })

        output$averagePace <- renderValueBox({
            box_text("avgPace", "Average pace",
                     icon = icon(create_icon('avgPace')))
        })

        between <- function(x, lower, upper) {
            if (any(is.na(c(x, lower, upper)))) {
                return(rep(TRUE, length(x)))
            }
            else {
                return((x >= lower) & (x <= upper))
            }
        }

        output$map <- renderLeaflet({
            sessionData <- reactive({
                choices <- between(data$summary$avgSpeed, input$average_speed[1], input$average_speed[2]) &
                    between(data$summary$avgHeartRate, input$average_heart_rate[1], input$average_heart_rate[2]) &
                    between(data$summary$distance, input$average_distance[1], input$average_distance[2])
                if (length(as.vector(data$selected_sessions))){
                    choices <- choices & (data$summary$session %in% as.vector(data$selected_sessions))
                }
                data$summary$session[choices]
            })
            shiny::validate(
                       need(sessionData(), 'Session data missing'),
                       need(data$trackeRdata_object, 'dataset missing'),
                       need(data$summary, 'summary missing')
                   )
            shiny_plot_map(data$trackeRdata_object,  sessionData(), data$summary)
        })

        lapply(data$metrics, function(i) {
            plotData <-  reactive({generate_graph_data(x = data$summary, what = i)})
            feature <- reactive({lab_sum(feature = i, data = data$summary)})
            units <- reactive({lab_sum(feature = i, data = data$summary,
                                       whole_text = FALSE)})
            output[[paste0(i)]] <- renderPlotly({
                if (all(is.na(plotData())) == TRUE) removeUI(selector = paste0("#box", i))
                if (all(is.na(plotData())) == FALSE) {
                    if (all(plotData()$value == 0) == TRUE) {
                        removeUI(selector = paste0("#box", i))
                    }
                }
                shiny::validate(need(!all(is.na(plotData())), 'No data'))
                plot_workouts(dat = plotData(), feature = feature(), name = i, units = units())
            })
        })

        ## DT
        output$summary <- DT::renderDataTable({
                                  data$hover <- event_data("plotly_selected")
                                  if (is.null(data$hover)) {
                                      data$selected_sessions <- data$summary$session
                                  }
                                  else {
                                      data$selected_sessions <- data$summary$session[na.omit(as.numeric(data$hover$key))]
                                  }
                                  dataSelected <- data.frame('Session' = data$summary[data$selected_sessions][["session"]],
                                                             'sessionStart' =
                                                                 format(data$summary[data$selected_sessions][["sessionStart"]],
                                                                        format = "%Y-%m-%d  %H:%M:%S"),
                                                             'sessionEnd' =
                                                                 format(data$summary[data$selected_sessions][["sessionEnd"]],
                                                                        format = "%Y-%m-%d %H:%M:%S"))
                                  req(is.data.frame(dataSelected))
                                  DT::datatable(dataSelected, rownames = FALSE, selection = 'none',
                                                options = list(dom='t', scrollY = "300px",
                                                               language =
                                                                   list(zeroRecords = "No workouts selected")
                                                               ))})
        output$cond <- reactive({
            TRUE
        })
        outputOptions(output, "cond", suspendWhenHidden = FALSE)
    })

    observeEvent(input$resetButton, {
        removeUI(selector = ".plots", immediate = TRUE, multiple = TRUE)
        data$selected_sessions <- data$summary$session
    })

    observeEvent(input$plotSelectedWorkouts, {
        output$cond <-reactive({
            FALSE
        })
        output$timeline_plot <- renderPlot({
            timeline(data$summary[data$selected_sessions])
        })
        for (i in c("pace", "heart.rate", "altitude", "work_capacity")) {
            insertUI(
                selector = ".content",
                where = "beforeEnd",
                ui = conditionalPanel(condition = "output.cond == false",
                                      div(class='plots', id=i, fluidRow(
                                                                   box(
                                                                       status = 'primary',
                                                                       width = 12,
                                                                       height = "350px",
                                                                       title = tagList(shiny::icon("gear"),
                                                                                       switch(i, "pace" = paste0("Pace"),
                                                                                              "heart.rate" = paste0("Heart Rate"),
                                                                                              "altitude" = paste0("Altitude"),
                                                                                              "work_capacity" = paste0("Work Capacity"))),
                                                                       div(style = 'overflow-x: scroll',
                                                                           plotlyOutput(paste0('plot_', i), width = if(length(data$selected_sessions) > 2)
                                                                                                                        paste0(toString(750*length(as.vector(data$selected_sessions))), 'px') else 'auto', height = "250px")))))))
        }

        lapply(c("pace", "heart.rate", "altitude"), function(i) {
            var_name_units <- reactive({lab_sum(feature = i,
                                                data = data$summary,
                                                transform_feature = FALSE)})
            var_units <- reactive({lab_sum(feature = i, data = data$summary,
                                           whole_text = FALSE,
                                           transform_feature = FALSE)})

            output[[paste0('plot_', i)]] <- renderPlotly({
                plot_selectedWorkouts(x = data$trackeRdata_object,
                                      session = data$selected_sessions,
                                      what = i, var_units = var_units(),
                                      var_name_units = var_name_units())
            })
        })

        output[[paste0('plot_', 'work_capacity')]] <- renderPlotly({
            if (all(is.na(data$summary$avgCadence)) == TRUE | all(is.na(data$summary$avgPower)) == TRUE) {
                removeUI(selector = paste0("#", 'work_capacity'))
            }
            shiny::validate(need(!all(is.na(data$summary$avgCadence)) & !all(is.na(data$summary$avgPower)), 'No data'))
            plot_work_capacity(run_data = data$trackeRdata_object, session = as.vector(data$selected_sessions))
        })

        ## Time in Zones
        insertUI(
            selector = ".content",
            where = "beforeEnd",
            ui = conditionalPanel(condition = "output.cond == false",
                                  div(class='plots',  id = 'zones', fluidRow(
                                                                        box(
                                                                            status = 'primary',
                                                                            width = 12,
                                                                            title = tagList(shiny::icon("gear"), 'Time in Zones'),
                                                                            selectizeInput('zones_for_plot', 'Select zone metrics to plot:', multiple = TRUE,
                                                                                           c('Altitude' = 'altitude',
                                                                                             'Speed' = 'speed',
                                                                                             'Pace' = 'pace'
                                                                                             ), selected = 'speed'
                                                                                           ),
                                                                            uiOutput('time_in_zones'))))))
        metrics_test_data <- observeEvent(input$zones_for_plot, {
            metrics <- c('Heart Rate' = 'heart.rate',
                         'Altitude' = 'altitude',
                         'Speed' = 'speed',
                         'Cadence' = 'cadence',
                         'Power' = 'power',
                         'Pace' = 'pace'
                         )
            available_data <- sapply(metrics, function(x) {
                class(try(zones(data$trackeRdata_object, what = x), silent=TRUE))[1] != "try-error"
            })
            updateSelectizeInput(session, 'zones_for_plot', choices = metrics[available_data], server = TRUE, selected='speed')
        }, once=TRUE)

        ## Reactive plot height based on number of sessions selected
        plot_height <- reactive({
            paste0(250 * length(input$zones_for_plot), 'px')
        })

        ## Render UI for plot
        output$time_in_zones <- renderUI({
            plotlyOutput('time_in_zone_plots', width = 'auto', height = plot_height())
        })

        ## Render actual plot
        output$time_in_zone_plots <- renderPlotly({
            plot_zones(run_data = data$trackeRdata_object, session = as.vector(data$selected_sessions), what = input$zones_for_plot)
        })

        ## Other metrics - Work capacity, Distribution profile, Concentration profile
        insertUI(
            selector = ".content",
            where = "beforeEnd",
            ui = conditionalPanel(condition = "output.cond == false",
                                  div(class='plots',  id = 'profiles', fluidRow(
                                                                           box(
                                                                               status = 'primary',
                                                                               width = 12,
                                                                               title = tagList(shiny::icon("gear"), 'Profiles'),
                                                                               selectizeInput('profile_metrics_for_plot', 'Concentration profiles:', multiple = TRUE,
                                                                                              c('Altitude' = 'altitude',
                                                                                                'Speed' = 'speed',
                                                                                                'Pace' = 'pace'
                                                                                                ), selected = 'speed'
                                                                                              ),
                                                                               uiOutput('profiles'))))))

        metrics_test_data <- observeEvent(input$profile_metrics_for_plot, {
            metrics <- c('Heart Rate' = 'heart.rate',
                         'Altitude' = 'altitude',
                         'Speed' = 'speed',
                         'Cadence' = 'cadence',
                         'Power' = 'power',
                         'Pace' = 'pace'
                         )
            available_data <- sapply(metrics, function(x) {
                class(try(zones(data$trackeRdata_object, what = x), silent=TRUE))[1] != "try-error"
            })
            updateSelectizeInput(session, 'profile_metrics_for_plot', choices = metrics[available_data], server = TRUE, selected='speed')
        }, once=TRUE)

        ## Reactive plot height based on number of sessions selected
        profile_plot_height <- reactive({
            paste0(250 * length(input$profile_metrics_for_plot), 'px')
        })

        ## Render UI for plot
        output$profiles <- renderUI({
            plotlyOutput('profiles_plots', width = 'auto', height = profile_plot_height())
        })

        ## Render actual plot
        output$profiles_plots <- renderPlotly({
            plot_profiles(run_data = data$trackeRdata_object, session = as.vector(data$selected_sessions), what = input$profile_metrics_for_plot)
        })
    })
}
