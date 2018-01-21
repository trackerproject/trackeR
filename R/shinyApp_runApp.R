#' Launch the trackeR dashboard
#'
#' @inheritParams shiny::runApp
#'
#' @export
trackeR_app <- function(quiet = FALSE) {

  shiny::runApp(appDir = system.file('shiny', package='trackeR'),
                launch.browser = TRUE, quiet = quiet)

}



