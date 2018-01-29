## Plot the work capacity W' (w prime).
##

plot_work_capacity <- function(data) {
  x <- Wprime(
    object = data$object, session = data$selected_sessions, quantity = "expended",
    cp = 4, version = "2012"
  )
  # Process data
  processed_data <- plot.trackeRWprime(x, shiny = TRUE)

  # Save session
  session_names <- data$selected_sessions
  df <- processed_data$df
  mylabels <- processed_data$mylabels
  df$Series <- as.factor(as.character(df$Series))
  df$id <- as.integer(factor(df$SessionID))
  df$numericDate <- as.numeric(df$Index)
  N <- nlevels(factor(df$id))

  ranges <- NULL
  for (i in unique(df$id)) {
    df_subset <- df[(df$id == i) & (df$Series == "movement"), ]

    values <- df_subset[, "Value"]
    no_values <- all(is.na(values))
    if (no_values) {
      current_range <- c(NA, NA)
    }
    else {
      current_range <- range(values, na.rm = TRUE)
    }
    ranges <- rbind(ranges, current_range)
  }
  if (na_ranges <- all(is.na(ranges))) {
    maximal_range <- c(-1, 1)
  } else {
    maximal_range <- c(min(ranges[, 1], na.rm = TRUE), max(ranges[, 2], na.rm = TRUE))
  }

  plot_stored <- vector("list", N)
  show_legend <- TRUE

  for (i in unique(df$id)) {
    df_subset <- df[(df$id == i) & (df$Series == "movement"), ]
    has_values <- !all(is.na(df_subset[, "Value"]) | df_subset[, "Value"] == 0)
    annotations_list <- list(
      text = paste("Sessions:", session_names[i]),
      xref = "paper",
      yref = "paper",
      yanchor = "bottom",
      xanchor = "center",
      align = "center",
      x = 0.5,
      y = 1,
      showarrow = FALSE
    )
    axis_list <- list(zeroline = FALSE)
    if (has_values) {
      a <- plotly::plot_ly(
        na.omit(df_subset), x = ~ Index, y = ~ Value,
        hoverinfo = "none",
        color = I("gray"), legendgroup = ~ Series,
        name = mylabels[1], showlegend = show_legend
      ) %>%
        plotly::add_lines(alpha = 0.4) %>%
        plotly::add_lines(
          data = na.omit(df[(df$id == i) & (df$Series == "wprime"), ]),
          x = ~ Index, y = ~ Value, hoverinfo = "text",
          text = ~ paste(round(Value, 2), "W'"),
          color = I("deepskyblue3"), legendgroup = ~Series, name = mylabels[2],
          showlegend = show_legend
        ) %>%
        plotly::layout(
          annotations = annotations_list,
          xaxis = axis_list, yaxis = c(axis_list, list(range = maximal_range * 1.02))
        )
    } else {
      df_subset$Value <- if (na_ranges) 0 else mean(maximal_range)
      a <- plotly::plot_ly(
        df_subset, x = ~ Index, y = ~ Value,
        hoverinfo = "none", type = "scatter",
        showlegend = show_legend
      ) %>%
        plotly::layout(
          annotations = annotations_list,
          xaxis = axis_list, yaxis = c(axis_list, list(
            range = maximal_range * 1.02,
            showticklabels = FALSE
          ))
        )
    }

    plot_stored[[i]] <- a
    show_legend <- FALSE
  }

  y <- list(title = "", fixedrange = TRUE)
  x <- list(title = "Time", fixedrange = TRUE)

  return(plotly::subplot(plot_stored, nrows = 1, shareY = TRUE, margin = 0.002) %>%
    plotly::config(displayModeBar = F) %>%
    plotly::layout(yaxis = y, xaxis = x, hovermode = "closest", legend = list(y = 1, orientation = "h")))
}
