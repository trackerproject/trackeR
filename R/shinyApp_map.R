## Plot all selected workouts on an interactive leaflet map.
##
## @param x An object of class \code{trackeRdata}.
## @param session A vector of selected sessions.
## @param data_summary An object of class \code{trackeRdataSummary}.

shiny_plot_map <- function(...){
  leafletRoute(shiny = TRUE,...)
}



