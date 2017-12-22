
server <- function(input, output, session) {

  # Metrics for cycling
  metrics <- c('Distance' = 'distance',
               'Duration' = 'duration',
               'Average speed' = 'avgSpeed',
               'Average pace' = 'avgPace',
               'Average cadence' = 'avgCadence',
               'Average heart rate' = 'avgHeartRate'
  )

  observeEvent(input$sportSelected, { if(input$sportSelected == 'running') {
    updateSelectizeInput(session, 'metricsSelected', choices = metrics, server = TRUE)
  }
  })
  # Disable selection of Power if sport is running

  #storing all data
  data <- reactiveValues()
  data$dataSelected <- NULL

  # directory
  shinyDirChoose(input, 'directory', roots = c(home = '~'),
                 filetypes = c('', "tcx", "db3", "json"))
  directory <- reactive(input$directory)
  # output$directory <- renderPrint(directory())

  path <- reactive({
    home <- normalizePath("~")
    file.path(home, paste(unlist(directory()$path[-1]),
                          collapse = .Platform$file.sep))
  })
  # observeEvent(
  #   ignoreNULL = TRUE,
  #   eventExpr = {
  #     input$directory
  #   },
  #   handlerExpr = {
  #     if (input$directory > 0) {
  #       # condition prevents handler execution on initial app launch
  #
  #       # launch the directory selection dialog with initial path read from cthe widget
  #       path = choose.dir(default = readDirectoryInput(session, 'directory'))
  #
  #       # update the widget value
  #       updateDirectoryInput(session, 'directory', value = path)
  #     }
  #   }
  # )

  observeEvent(input$changeUnits, {

    req(data$dataSet)
    get_selected_units <- function(feature){
      getUnits(data$dataSet)$unit[getUnits(data$dataSet)$variable %in% feature]
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
      getUnits(data$dataSet)$variable[!(getUnits(data$dataSet)$variable %in% unused_variables)]
      })

    change_units <- function(data){
      units <- c()
      for (i in allUnits()){
        units <- c(units, input[[paste0(i, 'Units')]])
      }
      data_updated <- changeUnits(data, variable = allUnits(), unit = units)

      return(data_updated)
    }

    data$dataSet <- change_units(data$dataSet)
    data$summary <- change_units(data$summary)
    removeModal()
  })

  observeEvent(input$uploadButton, {
    # show message if no file or folder selected
    if((is.null(input$processed_data_path$datapath)) && (path() == paste0(normalizePath("~"), '/'))){
      showModal(modalDialog(title = 'Important message',
                            div(tags$b("Please upload either processed or raw data",
                                       class='warningMessage')),
                            easyClose = TRUE,
                            size = 'm'))
    }
    shiny::req((is.null(input$processed_data_path$datapath) != TRUE) || (path() != paste0(normalizePath("~"), '/')))

    data$sport <- if (input$sportSelected == 'cycling') TRUE else FALSE


    if(path() == paste0(normalizePath("~"), '/')){
      data$dataSet <- readRDS(input$processed_data_path$datapath)
    } else if (is.null(input$processed_data_path$datapath) == TRUE) {
      data$dataSet <- callModule(readDirectory_shiny, "datafile", directory= path(),
                                 timezone = "GMT", cycling = data$sport)
    } else {
      raw_data <- reactive({callModule(readDirectory_shiny, "datafile", directory= path(),
                             timezone = "GMT", cycling = data$sport)})
      processed_data <- reactive({readRDS(input$processed_data_path$datapath)})
      req(raw_data())
      req(processed_data())
      data$dataSet <- c(processed_data(), raw_data())
    }

    output$download_data <- downloadHandler(

      filename = function() {
        paste0("data-", Sys.Date(), ".RData")
      },
      content = function(file) {
        saveRDS(data$dataSet, file)
        # save(data_download(), file)
      }
    )
    #data$fileType <- input$selectFileType



      # readDirectory(path(), timezone = "GMT",
      #                              cycling = data$sport)
    #using 'runs' dataset for testing
    # data("runs", package = "trackeR")

    # data$dataSet <- runs
    # data$dataSet <- runs
    data$summary <- summary(data$dataSet, session = 1:length(data$dataSet),
                            movingThreshold = 1)

  })

  observeEvent({input$updateUnits
                input$plotButton
    }, {

    shiny::req(data$dataSet)

    output$timeline_plot <- renderPlot({
      timeline(data$summary)
    })


    if (is.null(data$dataSet)) showModal(modalDialog(title = 'Important message',
                                                       div(tags$b("Please click upload data",
                                                                  class='warningMessage')),
                                                       easyClose = TRUE,
                                                       size = 'm'))

    # shiny::validate(
    #   need(data$dataSet, showModal(modalDialog(title = 'Not working')))
    # )


    removeUI(selector = ".plots", immediate = TRUE, multiple = TRUE)

    data$metrics <- input$metricsSelected
    data$button <- input$plotButton
    insertUI(
      selector = ".content",
      where = "beforeEnd",
      ui = conditionalPanel(condition = "output.cond == true", div(class='plots', fluidRow(
        box(
          status = 'primary',
          width = 12,
          height = "500px",
          title = tagList(shiny::icon("map"), 'Map'),
          withSpinner(leafletOutput('map', width = "auto", height = "430px"), size = 2),
          absolutePanel(top = 70, right = 60,
                        sliderInput("average_speed", "Average speed",
                                    floor(min(data$summary$avgSpeed)),
                                    ceiling(max(data$summary$avgSpeed)),
                                    value = c(floor(min(data$summary$avgSpeed)),
                                              ceiling(max(data$summary$avgSpeed))),
                                    step = 1, width = '200px'),
                        sliderInput("average_heart_rate", "Average heart rate",
                                    floor(min(data$summary$avgHeartRate, na.rm=TRUE)/10)*10,
                                    ceiling(max(data$summary$avgHeartRate, na.rm=TRUE)/10)*10,
                                    value = c(floor(min(data$summary$avgHeartRate, na.rm=TRUE)/10)*10,
                                              ceiling(max(data$summary$avgHeartRate, na.rm=TRUE)/10)*10),
                                    step = 10, width = '200px'),
                        sliderInput("average_distance", "Distance",
                                    floor(min(data$summary$distance)/1000)*1000,
                                    ceiling(max(data$summary$distance)/1000)*1000,
                                    value = c(floor(min(data$summary$distance)/1000)*1000,
                                              ceiling(max(data$summary$distance)/1000)*1000),
                                    step = 1000, width = '200px'),
                        actionButton('plotSelectedWorkouts', 'Plot selected workouts',
                                     style="color: #fff; background-color: #428bca;
                                     border-color:#428bca")

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

    output$averageDistance <- renderValueBox({
      text_output <- reactive({
        if (length(data$sessionsSelected) >= 1) {
            paste0(round(mean(data$dataSelected$distance, na.rm=TRUE), 0), ' ',
                   lab_sum('distance', data$summary, FALSE))
          # print('ble')
        } else {
           paste0(round(mean(data$summary$distance, na.rm=TRUE), 0), ' ',
                  lab_sum('distance', data$summary, FALSE))
          # print('bla')
        }
        })

      valueBox(text_output(), "Average Distance",
               icon = icon(create_icon('distance')),
               color = "light-blue")
    })

    output$averageDuration <- renderValueBox({

      text_output <- reactive({
        if (length(data$sessionsSelected) >= 1) {

          paste0(round(mean(data$dataSelected$duration, na.rm=TRUE), 0), ' ',
                 lab_sum('duration', data$summary, FALSE))

        } else {
          paste0(round(mean(data$summary$duration, na.rm=TRUE), 0), ' ',
                 lab_sum('duration', data$summary, FALSE))
          # print('bla')
        }
      })

      valueBox(text_output(), "Average Duration",
               icon = icon(create_icon('duration')), color = "light-blue")
    })

    output$averageHeartRate <- renderValueBox({
      text_output <- reactive({
        if (length(data$sessionsSelected) >= 1) {
          paste0(round(mean(data$dataSelected$avgHeartRate, na.rm=TRUE), 0), ' ',
                 lab_sum('avgHeartRate', data$summary, FALSE))
          # print('ble')
        } else {
          paste0(round(mean(data$summary$avgHeartRate, na.rm=TRUE), 0), ' ',
                 lab_sum('avgHeartRate', data$summary, FALSE))
          # print('bla')
        }
      })
      valueBox(text_output(), "Average Heart Rate",
               icon = icon(create_icon('avgHeartRate')), color = "light-blue")
    })

    output$averagePace <- renderValueBox({
      text_output <- reactive({
        if (length(data$sessionsSelected) >= 1) {
          paste0(round(mean(data$dataSelected$avgPace, na.rm=TRUE), 2), ' ',
                 lab_sum('avgPace', data$summary, FALSE))
          # print('ble')
        } else {
          paste0(round(mean(data$summary$avgPace, na.rm=TRUE), 2), ' ',
                 lab_sum('avgPace', data$summary, FALSE))
          # print('bla')
        }
      })
      valueBox(text_output(), "Average Pace",
               icon = icon(create_icon('avgPace')), color = "light-blue")
    })

    output$map <- renderLeaflet({
      sessionData <- reactive({

        if (length(as.vector(data$sessionsSelected)) == 0){
          data$summary$session[which(data$summary$avgSpeed >= input$average_speed[1]
                                     & data$summary$avgSpeed <= input$average_speed[2]
                                     & data$summary$avgHeartRate >= input$average_heart_rate[1]
                                     & data$summary$avgHeartRate <= input$average_heart_rate[2]
                                     & data$summary$distance >= input$average_distance[1]
                                     & data$summary$distance <= input$average_distance[2])]

        } else {
          data$summary$session[which(data$summary$avgSpeed >= input$average_speed[1]
                                     & data$summary$avgSpeed <= input$average_speed[2]
                                     & data$summary$avgHeartRate >= input$average_heart_rate[1]
                                     & data$summary$avgHeartRate <= input$average_heart_rate[2]
                                     & data$summary$distance >= input$average_distance[1]
                                     & data$summary$distance <= input$average_distance[2]
                                     & (data$summary$session %in% as.vector(data$sessionsSelected)))]
        }

      })
      shiny::validate(
        need(sessionData(), 'Session data missing'),
        need(data$dataSet, 'dataset missing'),
        need(data$summary, 'summary missing')
      )
      # req(sessionData())
      # req(data$dataSet)
      # req(data$summary)
      shiny_plot_map(data$dataSet,  sessionData(), data$summary)
    })

    lapply(data$metrics, function(i) {
      plotData <-  reactive({generateGraphs(x = data$summary, what = i)})
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

          plot_workouts(dat = plotData(), x = ~xaxis, y = ~value,
                        feature = feature(), name = i, units = units())
        })
    })

    output$summary <- DT::renderDataTable({
      data$hover <- event_data("plotly_selected")
      data$dataSelected <- data$summary[data$summary$session %in% data$hover$key[!is.na(data$hover$key )]]
      data$sessionsSelected <- data$dataSelected$session
      dataSelected <- data.frame('Session' = data$dataSelected$session,
                                 'sessionStart' =
                                   format(data$dataSelected$sessionStart,
                                          format = "%Y-%m-%d  %H:%M:%S"),
                                 'sessionEnd' =
                                   format(data$dataSelected$sessionEnd,
                                          format = "%Y-%m-%d %H:%M:%S"))
      req(is.data.frame(dataSelected))
      DT::datatable(dataSelected, rownames = FALSE, selection = 'none',
                    options = list(dom='t', scrollY = "300px",
                                   language =
                                     list(zeroRecords = "No workouts selected")
                    ))})
    #

    # output$selectedWorkout <- renderUI({
    #   actionButton('plotSelectedWorkouts', 'Plot selected workouts')
    # })

    output$cond <- reactive({
      TRUE
    })

    outputOptions(output, "cond", suspendWhenHidden = FALSE)

    # data$hover <- event_data("plotly_selected")
    # print(data$hover)
    # data$dataSelected <- data$summary[data$summary$session %in% data$hover$key[!is.na(data$hover$key )]]
    # data$sessionsSelected <- data$dataSelected$session
  })


  observeEvent(input$resetButton, {
    removeUI(selector = ".plots", immediate = TRUE, multiple = TRUE)


    output$summary <- renderTable({'No workouts selected'})
  })


  observeEvent(input$plotSelectedWorkouts, {
    #print(data$dataSelected)
    req(data$sessionsSelected)
    output$cond <-reactive({
      FALSE
    })

    for(i in c("pace", "heart.rate", "altitude", "work_capacity")){
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
              plotlyOutput(paste0('plot_', i), width = if(length(as.vector(data$sessionsSelected)) > 2)
                paste0(toString(750*length(as.vector(data$sessionsSelected))), 'px') else 'auto', height = "250px")))))))
    }

    lapply(c("pace", "heart.rate", "altitude"), function(i) {
      var_name_units <- reactive({lab_sum(feature = i,
                                          data = data$summary,
                                          transform_feature = FALSE)})
      var_units <- reactive({lab_sum(feature = i, data = data$summary,
                                     whole_text = FALSE,
                                     transform_feature = FALSE)})
      output[[paste0('plot_', i)]] <- renderPlotly({

        plot_selectedWorkouts(x = data$dataSet,
                              session = as.vector(data$sessionsSelected),
                              what = i, var_units = var_units(),
                              var_name_units = var_name_units())
      })
    })

    output[[paste0('plot_', 'work_capacity')]] <- renderPlotly({
      if (all(is.na(data$summary$avgCadence)) == TRUE | all(is.na(data$summary$avgPower)) == TRUE) {
        removeUI(selector = paste0("#", 'work_capacity'))
      }
      shiny::validate(need(!all(is.na(data$summary$avgCadence)) & !all(is.na(data$summary$avgPower)), 'No data'))
      plot_work_capacity(run_data = data$dataSet, session = as.vector(data$sessionsSelected))
    })

# Time in Zones
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
                                               c('Heart Rate' = 'heart.rate',
                                                 'Altitude' = 'altitude',
                                                 'Speed' = 'speed',
                                                 'Cadence' = 'cadence',
                                                 'Power' = 'power',
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
                          class(try(zones(data$dataSet, what = x), silent=TRUE))[1] != "try-error"
                          })
        updateSelectizeInput(session, 'zones_for_plot', choices = metrics[available_data], server = TRUE, selected='speed')
      }, once=TRUE)




# Reactive plot height based on number of sessions selected
    plot_height <- reactive({
      paste0(250 * length(input$zones_for_plot), 'px')
    })

# Render UI for plot
    output$time_in_zones <- renderUI({
      plotlyOutput('time_in_zone_plots', width = 'auto', height = plot_height())
    })

# Render actual plot
    output$time_in_zone_plots <- renderPlotly({



      plot_zones(run_data = data$dataSet, session = as.vector(data$sessionsSelected), what = input$zones_for_plot)
    })

# Other metrics - Work capacity, Distribution profile, Concentration profile
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
                                               c('Heart Rate' = 'heart.rate',
                                                 'Altitude' = 'altitude',
                                                 'Speed' = 'speed',
                                                 'Cadence' = 'cadence',
                                                 'Power' = 'power',
                                                 'Pace' = 'pace'
                                               ), selected = 'speed'
                                ),
                                uiOutput('profiles'))))))

# Reactive plot height based on number of sessions selected
    profile_plot_height <- reactive({
      paste0(250 * length(input$profile_metrics_for_plot), 'px')
    })

# Render UI for plot
    output$profiles <- renderUI({
      plotlyOutput('profiles_plots', width = 'auto', height = profile_plot_height())
    })

# Render actual plot
    output$profiles_plots <- renderPlotly({
       plot_profiles(run_data = data$dataSet, session = as.vector(data$sessionsSelected), what = input$profile_metrics_for_plot)
    })
  })
}
