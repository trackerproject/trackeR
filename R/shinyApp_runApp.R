#' @import shinydashboard
#' @import shiny
#' @import plotly
#' @import leaflet
#' @import mgcv
#' @import shinycssloaders
#' @import shinyFiles
#' @import DT
#' @import plyr

startApp <- function() {

  shiny::runApp(appDir = system.file('shiny', package='trackeR'),
                launch.browser = TRUE)

}



