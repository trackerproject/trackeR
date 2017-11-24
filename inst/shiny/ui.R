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
    tags$head(tags$style(".warningMessage {
                          font-size: 20px;
                         }
                         hr {
                         border-top: 1px solid;
                         }
                          a#download_data {
                            color: #333;
                          }
                         ")),
    sidebarMenu(
      menuItem('Dashboard', tabName = 'dashboard', icon = icon('dashboard')),
      hr(),
      fileInput('processed_data_path', 'Load processed data (optional)'),
      tags$div(class = 'form-group shiny-input-container', tags$label('Add raw data (optional)'),
              tags$div(class = 'input-group', shinyDirButton("directory", "Select Folder", "Upload"))),
      selectInput('sportSelected', 'Select sport:', multiple = FALSE,
                  c('Cycling' = 'cycling',
                    'Running' = 'running')
      ),
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
      div(div(style="display: inline-block;vertical-align:top; width: 120px;", actionButton('changeUnits', 'Change units')),
          div(style="display: inline-block;vertical-align:top; width: 100px;", actionButton('resetButton', 'Reset'))),
      hr(),
      
      # tags$div(class = "add_raw_files", tags$title('Add raw files'), 
      #          tags$div(class = 'load_data', 
      #                   shinyDirButton("directory", "Add raw data", "Upload")))
      div(div(style="display: inline-block;vertical-align:top; width: 100px;", actionButton('uploadButton', 'Upload', icon("upload"), 
                  style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
      
      div(style="display: inline-block;vertical-align:top; width: 100px;", actionButton('plotButton', 'Plot', icon('area-chart'),
                   style="color: #fff; background-color: #3CB371; border-color: #3CB371"))),
      hr(),
      tags$div(class = 'form-group shiny-input-container', tags$label('Download data'),
               tags$div(class = 'input-group', downloadButton('download_data', 'Download procesed data')))
      
    )

  ),
  dashboardBody(
    tabItems(
     
    # # javascript code to send darewta to shiny server
    # #htmlTemplate('component.html'),
    #   box(
    #     status = 'info',
    #     width = 6,
    #     height = "250px",
    #     title = tagList(shiny::icon("gear"), "Selector"),
    # 
    #     # selectInput('selectFileType', 'Select type of GPS container file', multiple = FALSE,
    #     #             c('TCX' = "tcx",
    #     #               'db3' = "db3",
    #     #               'JSON' = "json")
    #     # ),
    #     # fillRow(shinyDirButton("directory", "Chose a directory", "Upload"), width = '20%'),
    # 
    #     #directoryInput('directory', label = 'Select a directory:', value = '~/'),
    #     
    #     verbatimTextOutput('directory_table')
    #     
    #     # HTML(
    #     #      <label>Select date range</label>
    #     #      <div id='reportrange' class='pull-right' style='background: #fff; cursor: pointer; padding: 5px 10px;                                    #       border: 1px solid #ccc; width: 100%; margin-bottom: 10px'>
    #     #      <i class='glyphicon glyphicon-calendar fa fa-calendar'></i>&nbsp;
    #     #      <span id='dateRange' type='text'></span> <b class='caret'></b>
    #     #      </div>
    #     #      "),
    #     
    # 
    #     # fileInput(
    #     #   'file', 'Choose file', multiple = TRUE),
    # 
    #     # tags$script('
    #     #          document.getElementById("plotButton").onclick = function(){
    #     #          var date = document.getElementById("dateRange").innerHTML;
    #     #          Shiny.onInputChange("dateSelected", date);
    #     #          };
    #     #  ')
    # box(
    #   status = 'info',
    #   width = 6,
    #   height = "250px",
    #   title = tagList(shiny::icon("reorder"), "Summary of selected workouts"),
    #   #verbatimTextOutput("result"),
    #   #verbatimTextOutput("hover"),
    #   tableOutput('summary'),
    #   uiOutput("selectedWorkout")
    # )
       
    tabItem(tabName = 'dashboard',
      fluidRow(
      box(
        status = 'primary',
        width = 6,
        height = "200px",
        title = tagList(shiny::icon("reorder"), "Summary of selected workouts"),
        #verbatimTextOutput("result"),
        #verbatimTextOutput("hover"),
        div(style = 'overflow-y: scroll', DT::dataTableOutput('summary'))
      )))
    
  )
  )
  )

