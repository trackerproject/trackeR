## app.R ##
# file.sources = list.files(pattern="*.R")
# sapply(file.sources,source,.GlobalEnv)
# library(shiny)
# library(shinydashboard)
# library(plotly)
# library(leaflet)
# library(mgcv)
# source('directoryInput.R')
# library("zoo")
# library("trackeR")
# source('modules.R')
# source('map.R')
# source('plotSelectedWorkouts.R')

metrics <- c('Distance' = 'distance',
             'Duration' = 'duration',
             'Average speed' = 'avgSpeed',
             'Average pace' = 'avgPace',
             'Average cadence' = 'avgCadence',
             'Average power' = 'avgPower',
             'Average heart rate' = 'avgHeartRate',
             'Work to rest ratio' = 'wrRatio'
)

ui <- dashboardPage(skin = 'black',
  dashboardHeader(title = 'TrackerR'),
  dashboardSidebar(
    disable = TRUE
  ),
  dashboardBody(
    tags$head(tags$style(".warningMessage{
                                 font-size: 30px;
                         }")),
    # javascript code to send darewta to shiny server
    #htmlTemplate('component.html'),
    fluidRow(
      box(
        status = 'info',
        width = 6,
        height = "350px",
        title = tagList(shiny::icon("gear"), "Selector"),
        shinyDirButton("directory", "Chose a directory", "Upload"),
        selectInput('sportSelected', 'Select sport:', multiple = FALSE,
                    c('Cycling' = 'cycling',
                      'Running' = 'running')
        ),
        # selectInput('selectFileType', 'Select type of GPS container file', multiple = FALSE,
        #             c('TCX' = "tcx",
        #               'db3' = "db3",
        #               'JSON' = "json")
        # ),
        # fillRow(shinyDirButton("directory", "Chose a directory", "Upload"), width = '20%'),

        #directoryInput('directory', label = 'Select a directory:', value = '~/'),
        actionButton('uploadButton', 'Upload'),
        verbatimTextOutput('directory_table'),
        
        # HTML(
        #      <label>Select date range</label>
        #      <div id='reportrange' class='pull-right' style='background: #fff; cursor: pointer; padding: 5px 10px;                                    #       border: 1px solid #ccc; width: 100%; margin-bottom: 10px'>
        #      <i class='glyphicon glyphicon-calendar fa fa-calendar'></i>&nbsp;
        #      <span id='dateRange' type='text'></span> <b class='caret'></b>
        #      </div>
        #      "),
        
        selectizeInput('metricsSelected', 'Select metrics:', multiple = TRUE,
                       c('Distance' = 'distance',
                         'Duration' = 'duration',
                         'Average speed' = 'avgSpeed',
                         'Average pace' = 'avgPace',
                         'Average cadence' = 'avgCadence',
                         'Average power' = 'avgPower',
                         'Average heart rate' = 'avgHeartRate',
                         'Work to rest ratio' = 'wrRatio'
                       )
        ),
        # fileInput(
        #   'file', 'Choose file', multiple = TRUE),
        actionButton('plotButton', 'Plot'),
        actionButton('changeUnits', 'Change units'),
        actionButton('resetButton', 'Reset')
        # tags$script('
        #          document.getElementById("plotButton").onclick = function(){
        #          var date = document.getElementById("dateRange").innerHTML;
        #          Shiny.onInputChange("dateSelected", date);
        #          };
        #  ')
      ),
      box(
        status = 'info',
        width = 6,
        height = "350px",
        title = tagList(shiny::icon("reorder"), "Summary of selected workouts"),
        #verbatimTextOutput("result"),
        #verbatimTextOutput("hover"),
        tableOutput('summary'),
        uiOutput("selectedWorkout")
      )
    )
  )
)
