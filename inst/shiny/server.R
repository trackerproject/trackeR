
server <- function(input, output, session) {
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

    data("runs", package = "trackeR")
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
                                    floor(min(data$summary$avgHeartRate)/10)*10, 
                                    ceiling(max(data$summary$avgHeartRate)/10)*10,
                                    value = c(floor(min(data$summary$avgHeartRate)/10)*10,
                                              ceiling(max(data$summary$avgHeartRate)/10)*10), 
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
                              div(class='plots', fluidRow(
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
            paste0(round(mean(data$dataSelected$distance), 0), ' ', 
                   lab_sum('distance', data$summary, FALSE))
          # print('ble')
        } else {
           paste0(round(mean(data$summary$distance), 0), ' ', 
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
         
          paste0(round(mean(data$dataSelected$duration), 0), ' ', 
                 lab_sum('duration', data$summary, FALSE))
          
        } else {
          paste0(round(mean(data$summary$duration), 0), ' ', 
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
          paste0(round(mean(data$dataSelected$avgHeartRate), 0), ' ', 
                 lab_sum('avgHeartRate', data$summary, FALSE))
          # print('ble')
        } else {
          paste0(round(mean(data$summary$avgHeartRate), 0), ' ', 
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
          paste0(round(mean(data$dataSelected$avgPace), 2), ' ', 
                 lab_sum('avgPace', data$summary, FALSE))
          # print('ble')
        } else {
          paste0(round(mean(data$summary$avgPace), 2), ' ', 
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
    
    for(i in c("pace", "heart.rate", 'altitude')){
    insertUI(
      selector = ".content",
      where = "beforeEnd",
      ui = conditionalPanel(condition = "output.cond == false", 
                            div(class='plots', fluidRow(
        box(
          status = 'primary',
          width = 12,
          height = "350px",
          title = tagList(shiny::icon("gear"), 
                          switch(i, "pace" = paste0("Pace"),
                                "heart.rate" = paste0("Heart Rate"),
                                "altitude" = paste0("Altitude"))),
          div(style = 'overflow-x: scroll', 
              plotlyOutput(paste0('plot_', i), width = if(length(as.vector(data$sessionsSelected)) > 2) 
                paste0(toString(750*length(as.vector(data$sessionsSelected))), 'px') else 'auto', height = "250px")))))))
    }
    
    lapply(c("pace", "heart.rate", 'altitude'), function(i) {
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
                                               ), selected = 'heart.rate'
                                ),
                                uiOutput('time_in_zones'))))))
    
    plot_height <- reactive({
      paste0(250 * length(input$zones_for_plot), 'px')
    })
    

    output$time_in_zones <- renderUI({
      plotlyOutput('time_in_zone_plots', width = 'auto', height = plot_height())
    })
      
    output$time_in_zone_plots <- renderPlotly({
      plot_zones(run_data = data$dataSet, session = as.vector(data$sessionsSelected), what = input$zones_for_plot)
    })
    
    
  })

}
