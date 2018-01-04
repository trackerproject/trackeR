## app.R ##
## file.sources = list.files(pattern="*.R")
## sapply(file.sources,source,.GlobalEnv)
## library(shiny)
## library(shinydashboard)
## library(plotly)
## library(leaflet)
## library(mgcv)
## source('directoryInput.R')
## library("zoo")
## library("trackeR")
## source('modules.R')
## source('map.R')
## source('plotSelectedWorkouts.R')


ui <- dashboardPage(
    skin = 'black',
    dashboardHeader(title = 'TrackerR'),
    dashboardSidebar(
        tags$head(tags$script("
                            function hideElement(i) {
                                var x = document.getElementById(i);
                                x.style.display = 'none';
                            }
                           ")),
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
            fileInput('processed_data_path', 'Load processed data'),
            tags$div(class = 'form-group shiny-input-container', tags$label('Add raw data'),
                     tags$div(class = 'input-group', shinyDirButton("directory", "Select Folder", "Upload"))),
            div(div(style="display: inline-block;vertical-align:top; width: 100px;", actionButton('uploadButton', 'Upload', icon("upload"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                hr(),
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
                div(style="display: inline-block;vertical-align:top; width: 100px;", actionButton('plotButton', 'Plot', icon('area-chart'), style="color: #fff; background-color: #3CB371; border-color: #3CB371"))),
            hr(),
            tags$div(class = 'form-group shiny-input-container', tags$label('Download data'),
                     tags$div(class = 'input-group', downloadButton('download_data', 'Download procesed data')))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = 'dashboard',
                    fluidRow(
                        box(
                            status = 'primary',
                            width = 6,
                            height = "420px",
                            title = tagList(shiny::icon("reorder"), "Summary of selected workouts"),
                            div(style = 'overflow-y: scroll', DT::dataTableOutput('summary'))
                        ),
                        box(
                            status = 'primary',
                            width = 6,
                            height = '420px',
                            title = tagList(shiny::icon('calendar', lib='glyphicon'), 'Workout Timeline'),
                            plotOutput('timeline_plot', width = "100%", height = "350px")
                        )
                    ))
        )
    )
)


