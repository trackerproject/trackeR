#' Render data in summary box
#' @param short_name A character. The metric name, e.g., distance.
#' @param long_name A character. The title of the box.
#' @param data An object of class \code{reactivevalues}.
render_summary_box <- function(short_name, long_name, data) {
  box_text <- function(what, subtitle, icon, data) {
    value <- reactive({
      value <- data$summary[data$selectedSessions][[what]]
      value <- round(mean(value[is.finite(value)], na.rm = TRUE), 1)
      if (is.na(value)) {
        "not available"
      }
      else {
        paste0(value, " ", lab_sum(what, data$summary, FALSE))
      }
    })
    color <- if (value() == "not available") "olive" else "light-blue"
    shinydashboard::valueBox(value(), subtitle, icon, color = color)
  }

  shinydashboard::renderValueBox({
    box_text(
      what = short_name,
      subtitle = long_name,
      icon = icon(create_icon(short_name)),
      data = data
    )
  })
}

#' Generate an object with selected sessions
#' @param data An object of class \code{reactivevalues}.
#' @param input A shiny object with user input.
#' @param plot_selection A logical. Whether session selection made from a plot.
#' @param sport_selection A logical. Whether session selection made from sport selector.
#' @param table_selection A logical. Whether session selection made from the summary table.
#' @param no_selection A logical. Whether no sessions are selected.
generate_selected_sessions_object <- function(data, input, 
                                              plot_selection = FALSE,
                                              sport_selection = FALSE, 
                                              table_selection = FALSE,
                                              no_selection = FALSE) {
  
  data$hover <- plotly::event_data("plotly_selected")
  if (sport_selection) {
    data$selectedSessions <- data$summary$session[sport(data$object) %in% input$sports]
  }
  if (plot_selection) {
    data$selectedSessions <- unique(na.omit(as.numeric(data$hover$key)))
  }
  if (table_selection) {
    data$selectedSessions <- input$summary_rows_selected
  }
  if (no_selection) {
    data$selectedSessions <- data$summary$session
  }
  if (length(data$selectedSessions) == 0) {
    data$selectedSessions <- data$summary$session
  }
  data$selectedSessions <- sort(data$selectedSessions)
}

#' Render summary table
#' @param data An object of class \code{reactivevalues}.
#' @param input A shiny object with user input.
render_summary_table <- function(data, input) {
  DT::renderDataTable({
    dataSelected <- data.frame(
      "Session" = data$summary[["session"]],
      "Date" =
        format(
          data$summary[["sessionStart"]],
          format = "%A, %B %d, %Y"
        ),
      "Start" =
        format(
          data$summary[["sessionStart"]],
          format = "%H:%M"
        ),
      "End" =
        format(
          data$summary[["sessionEnd"]],
          format = "%H:%M"
        ),
      "Duration" =
        paste(
          round(data$summary[["duration"]], 1),
          lab_sum("duration", data = data$summary, whole_text = FALSE)
        ),
      "Sport" =
        sport(data$object)
    )
    DT::datatable(
      dataSelected,
      rownames = FALSE,
      # selection = "none",
      autoHideNavigation = TRUE,
      options = list(paging = FALSE, scrollY = "295px", info = FALSE)
    )
  })
}
