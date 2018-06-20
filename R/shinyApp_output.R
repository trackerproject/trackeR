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
    shinydashboard::valueBox(value(), subtitle, icon, color = if (value() == "not available") "olive" else "light-blue")
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

#' Render summary table
#' @param data An object of class \code{reactivevalues}.
#' @param input A shiny object with user input.
render_summary_table <- function(data, input) {
  DT::renderDataTable({
    if (!is.null(input$sports)) {
      sessions_by_sport <- data$summary$session[sport(data$object) %in% input$sports]
    } else {
      sessions_by_sport <- data$summary$session
    }

    data$hover <- plotly::event_data("plotly_selected")
    if (!is.null(data$summary)) {
      if (is.null(data$hover) | length(data$hover) == 0) {
        data$selectedSessions <- intersect(data$summary$session, sessions_by_sport)
      }
      else {
        # data$selectedSessions <- data$summary$session[na.omit(as.numeric(data$hover$key))]
        data$selectedSessions <- intersect(
          unique(na.omit(as.numeric(data$hover$key))),
          sessions_by_sport
        )
      }
      dataSelected <- data.frame(
        "Session" = data$summary[data$selectedSessions][["session"]],
        "Date" =
          format(
            data$summary[data$selectedSessions][["sessionStart"]],
            format = "%A, %B %d, %Y"
          ),
        "Start" =
          format(
            data$summary[data$selectedSessions][["sessionStart"]],
            format = "%H:%M"
          ),
        "End" =
          format(
            data$summary[data$selectedSessions][["sessionEnd"]],
            format = "%H:%M"
          ),
        "Duration" =
          paste(
            round(data$summary[data$selectedSessions][["duration"]], 1),
            lab_sum("duration", data = data$summary, whole_text = FALSE)
          ),
        "Sport" =
          sport(data$object[data$selectedSessions])
      )
      DT::datatable(
        dataSelected,
        rownames = FALSE,
        selection = "none",
        autoHideNavigation = TRUE,
        options = list(paging = FALSE, scrollY = "295px", info = FALSE)
      )
    }
  })
}
