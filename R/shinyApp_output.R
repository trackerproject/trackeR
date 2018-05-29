#' Render data in summary box
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
render_summary_table <- function(data, input) {
  DT::renderDataTable({
      if(!is.null(input$sports)){
        sessions_by_sport <- data$summary$session[data$classification %in% input$sports]
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
          data$selectedSessions <- intersect(unique(na.omit(as.numeric(data$hover$key))),
                                             sessions_by_sport)
        }
        dataSelected <- data.frame(
          "Session" = data$summary[data$selectedSessions][["session"]],
          "sessionStart" =
            format(
              data$summary[data$selectedSessions][["sessionStart"]],
              format = "%Y-%m-%d  %H:%M:%S"
            ),
          "sessionEnd" =
            format(
              data$summary[data$selectedSessions][["sessionEnd"]],
              format = "%Y-%m-%d %H:%M:%S"
            )
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
