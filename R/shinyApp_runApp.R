trackeR_app <- function() {

  shiny::runApp(appDir = system.file('shiny', package='trackeR'),
                launch.browser = TRUE)

}



