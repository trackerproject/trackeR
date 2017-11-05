
server <- function(input, output, session) {
  #storing all data
  data <- reactiveValues()
  data$dataSelected <- NULL
  # observeEvent(
  #   ignoreNULL = TRUE,
  #   eventExpr = {
  #     input$directory
  #   },
  #   handlerExpr = {
  #     if (input$directory > 0) {
  #       # condition prevents handler execution on initial app launch
  #       
  #       # launch the directory selection dialog with initial path
  #       path = choose.dir(default = readDirectoryInput(session, 'directory'))
  #       # update the widget value
  #       updateDirectoryInput(session, 'directory', value = path)
  #     }
  #   })
  
  observeEvent(input$uploadButton, {
    data$sport <- if (input$sportSelected == 'cycling') TRUE else FALSE
    #data$filePath <- readDirectoryInput(session, 'directory')
    data$fileType <- input$selectFileType
    #data$dataSet <- readDirectory(data$filePath, timezone = "GMT", 
    #                               cycling = data$sport)
    #using 'runs' dataset for testing
    data("runs", package = "trackeR")
    data$dataSet <- runs
    data$summary <- summary(data$dataSet, session = 1:length(data$dataSet),
                            movingThreshold = 1)
  })
  
  observeEvent(input$plotButton, {
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
          leafletOutput('map', width = "auto", height = "430px"),
          absolutePanel(top = 70, right = 60,
                        sliderInput("average_speed", "Average speed", floor(min(data$summary$avgSpeed)), 
                                    ceiling(max(data$summary$avgSpeed)),
                                    value = c(floor(min(data$summary$avgSpeed)), ceiling(max(data$summary$avgSpeed))), 
                                    step = 1, width = '200px'),
                        sliderInput("average_heart_rate", "Average heart rate", floor(min(data$summary$avgHeartRate)/10)*10, 
                                    ceiling(max(data$summary$avgHeartRate)/10)*10,
                                    value = c(floor(min(data$summary$avgHeartRate)/10)*10, ceiling(max(data$summary$avgHeartRate)/10)*10), 
                                    step = 10, width = '200px'),
                        sliderInput("average_distance", "Distance", floor(min(data$summary$distance)/1000)*1000, 
                                    ceiling(max(data$summary$distance)/1000)*1000,
                                    value = c(floor(min(data$summary$distance)/1000)*1000, ceiling(max(data$summary$distance)/1000)*1000), 
                                    step = 1000, width = '200px')
                        ))))))
    
    insertUI(
      selector = ".content",
      where = "beforeEnd",
      ui = conditionalPanel(condition = "output.cond == true", div(class='plots', fluidRow(
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
        ui = conditionalPanel(condition = "output.cond == true", div(class='plots', fluidRow(
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
            paste0(round(mean(data$dataSelected$distance), 0), ' ', lab_sum('distance', data$summary, FALSE))
          # print('ble')
        } else {
           paste0(round(mean(data$summary$distance), 0), ' ', lab_sum('distance', data$summary, FALSE))
          # print('bla')
        }
        }) 
   
      valueBox(text_output(), "Average Distance", icon = icon(create_icon('distance')), color = "light-blue")
    })
    
    output$averageDuration <- renderValueBox({
      print(nrow(data$dataSelected))
      text_output <- reactive({
        if (length(data$sessionsSelected) >= 1) {
         
          paste0(round(mean(data$dataSelected$duration), 0), ' ', lab_sum('duration', data$summary, FALSE))
          
        } else {
          paste0(round(mean(data$summary$duration), 0), ' ', lab_sum('duration', data$summary, FALSE))
          # print('bla')
        }
      })
  
      valueBox(text_output(), "Average Duration", icon = icon(create_icon('duration')), color = "light-blue")
    })
    
    output$averageHeartRate <- renderValueBox({
      text_output <- reactive({
        if (length(data$sessionsSelected) >= 1) {
          paste0(round(mean(data$dataSelected$avgHeartRate), 0), ' ', lab_sum('avgHeartRate', data$summary, FALSE))
          # print('ble')
        } else {
          paste0(round(mean(data$summary$avgHeartRate), 0), ' ', lab_sum('avgHeartRate', data$summary, FALSE))
          # print('bla')
        }
      })
      valueBox(text_output(), "Average Heart Rate", icon = icon(create_icon('avgHeartRate')), color = "light-blue")
    })
    
    output$averagePace <- renderValueBox({
      text_output <- reactive({
        if (length(data$sessionsSelected) >= 1) {
          paste0(round(mean(data$dataSelected$avgPace), 2), ' ', lab_sum('avgPace', data$summary, FALSE))
          # print('ble')
        } else {
          paste0(round(mean(data$summary$avgPace), 2), ' ', lab_sum('avgPace', data$summary, FALSE))
          # print('bla')
        }
      })
      valueBox(text_output(), "Average Pace", icon = icon(create_icon('avgPace')), color = "light-blue")
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
      shiny_plot_map(data$dataSet,  sessionData(), data$summary)
    })
    
    lapply(data$metrics, function(i) {
      plotData <-  reactive({generateGraphs(x = data$summary, what = i)})
      feature <- reactive({lab_sum(feature = i, data = data$summary)})
      units <- reactive({lab_sum(feature = i, data = data$summary, whole_text = FALSE)})
      print(head(plotData()))
      output[[paste0(i)]] <- renderPlotly({
        plot_workouts(dat = plotData(), x = ~xaxis, y = ~value, feature = feature(), name = i, units = units())
      })
    })
    
    output$summary <- renderTable({
      data$hover <- event_data("plotly_selected")
      data$dataSelected <- data$summary[data$summary$session %in% data$hover$key[!is.na(data$hover$key )]]
      data$sessionsSelected <- data$dataSelected$session
      dataSelected <- data.frame('Session' = data$dataSelected$session,
                                 'sessionStart' = format(data$dataSelected$sessionStart,
                                                         format = "%Y-%m-%d  %H:%M:%S"),
                                 'sessionEnd' = format(data$dataSelected$sessionEnd, 
                                                       format = "%Y-%m-%d %H:%M:%S"))
      if (nrow(dataSelected) == 0) 'No workouts selected' else dataSelected}, hover = TRUE, align = 'l',  width = '100%'
    )
    
    output$selectedWorkout <- renderUI({
      actionButton('plotSelectedWorkouts', 'Plot selected workouts')
    })
    
    output$cond <- reactive({
      TRUE
    })
    
    outputOptions(output, "cond", suspendWhenHidden = FALSE)
  })
  
  
  observeEvent(input$resetButton, {
    removeUI(selector = ".plots", immediate = TRUE, multiple = TRUE)
    output$summary <- renderTable({'No workouts selected'})
  })
  

  observeEvent(input$plotSelectedWorkouts, {
    #print(data$dataSelected)
    output$cond <-reactive({
      FALSE
    })
    
    insertUI(
      selector = ".content",
      where = "beforeEnd",
      ui = conditionalPanel(condition = "output.cond == false", div(class='plots', fluidRow(
        box(
          status = 'primary',
          width = 12,
          height = "500px",
          title = tagList(shiny::icon("gear"), 'Individual Workouts'),
          div(style = 'overflow-x: scroll', 
              plotlyOutput('plotIndividualWorkouts', width = if(length(as.vector(data$sessionsSelected)) > 2) 
                paste0(toString(750*length(as.vector(data$sessionsSelected))),'px') else 'auto', height = "400px")))))))
    
    output$plotIndividualWorkouts <- renderPlotly({
      plot_selectedWorkouts(data$dataSet, as.vector(data$sessionsSelected), 'heart.rate')
    })
  })
  
  
}
