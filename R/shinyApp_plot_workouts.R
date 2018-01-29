## A time-series plotly graph for a given variable (e.g. "avgHeartRate").
##
## @param dat A dataframe generated using \code{generate_graph_data()}.
## @param feature A character for y-axis title generated using \code{lab_sum()}.
## @param what A character for the feature to be plotted (e.g. "avgHeartRate").
## @param units A character of the unit of measurement for the given variable (e.g. "[bmp]") generated using \code{lab_sum()}.

plot_workouts <- function(x, what, data) {
  feature <- reactive({
    lab_sum(feature = what, data = data$summary)
  })
  units <- reactive({
    lab_sum(
      feature = what, data = data$summary,
      whole_text = FALSE
    )
  })
  dat <- plot.trackeRdataSummary(
    x = x, what = what, date = TRUE, group = c("total"),
    shiny = TRUE
  )

  d <- plotly::event_data("plotly_selected")
  what <- switch(what,
    "distance" = "Distance",
    "duration" = "Duration",
    "avgSpeed" = "Average Speed",
    "avgPace" = "Average Pace",
    "avgCadence" = "Average Cadence",
    "avgPower" = "Average Power",
    "avgHeartRate" = "Average Heart Rate",
    "wrRatio" = "work-to-rest ratio"
  )
  # print(d)
  p <- plotly::plot_ly(
    dat, x = ~xaxis, y = ~value, hoverinfo = "text",
    text = ~paste(
      "Date:", format(sessionStart, format = "%Y-%m-%d"),
      "\n", what, ":", round(value, 2), units()
    )
  ) %>%
    plotly::add_markers(key = dat$session, color = I("deepskyblue3")) %>%
    plotly::add_lines(color = I("deepskyblue2"))
  if (length(d[["key"]]) > 0) {
    m <- dat[dat$session %in% d[["key"]], ]
    p <- plotly::add_markers(p, data = m, color = I("darkorange3")) %>%
      plotly::add_lines(data = m, color = I("darkorange2"))
  }
  y <- list(title = feature())
  x <- list(title = "Date")

  plotly::layout(p, dragmode = "select", showlegend = FALSE, yaxis = y, xaxis = x, margin = list(l = 80, b = 50, pad = 0))
}
