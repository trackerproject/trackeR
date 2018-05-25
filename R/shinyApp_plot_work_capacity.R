#' Plot the work capacity W' (w prime).
#'
#' @param x An object of class \code{\link{trackeRdata}}.
#' @param session A numeric vector of the sessions to be used, defaults to all sessions.
#' @param plotly A boolean as to whether an output should be a plotly plot or a standard TrackeR plot.
#' @param dates Logical. Should the date of the session be used in the panel header?
#' @param scaled Logical. Should the W' be scaled to the movement variable (power or speed)
#'     which is then plotted in the background?
#'
plot_work_capacity <- function(x, session, plotly=TRUE, dates = TRUE, scaled = TRUE) {
  x <- Wprime(
    object = x, session = session, quantity = "expended",
    cp = 4, version = "2012"
  )

  if (plotly) {
    Series <- NULL

    quantity <- attr(x, "quantity")
    cp <- attr(x, "cp")
    cycling <- attr(x, "cycling")
    Wunit <- if (cycling)
        "[J]" else "[m]"
    mylabels <- c(paste0(ifelse(cycling, "Power", "Speed"), " [", prettifyUnits(attr(x,
        "unit")$unit), "]"), paste("W'", quantity, "[scaled]"))

    ## select sessions
    if (is.null(session)) session <- seq_along(x)

    ## transform W' to match power/speed scale
    if (scaled) {
        sdMov <- stats::sd(unlist(lapply(x, function(z) z$movement)), na.rm = TRUE)
        mMov <- mean(unlist(lapply(x, function(z) z$movement)), na.rm = TRUE)

        x <- lapply(x, function(z) {
            w <- (coredata(z$wprime) - mean(coredata(z$wprime), na.rm = TRUE))/stats::sd(coredata(z$wprime),
                na.rm = TRUE)
            w <- w * sdMov  #sd(coredata(z$movement), na.rm = TRUE)
            z$wprime <- w + mMov
            # max(mMov, abs(min(w, na.rm = TRUE))) max(mean(coredata(z$movement), na.rm = TRUE),
            # abs(min(w, na.rm = TRUE)))
            return(z)
        })
    }
    ## get data
    class(x) <- "trackeRWprime"

    df <- fortify(x, melt = TRUE)

    ## prepare session id for panel header
    if (dates) {
        df$SessionID <- format(session[df$SessionID])
        df$SessionID <- gsub(" ", "0", df$SessionID)
        df$SessionID <- paste(df$SessionID, format(df$Index, "%Y-%m-%d"), sep = ": ")
    } else {
        df$SessionID <- factor(df$SessionID, levels = seq_along(session), labels = session)
    }
    df$Series <- factor(df$Series)

    ## check that there is data to plot
    for (l in levels(df$Series)) {
        if (all(is.na(subset(df, Series == l, select = "Value"))))
            df <- df[!(df$Series == l), ]
    }
    # Save session
    session_names <- session
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
    y_axis_range <- c(min(na.omit(df[df$Series == "wprime", 'Value'])) * 0.8,
                      max(na.omit(df[df$Series == "wprime", 'Value'])) * 1.2)

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

    y <- list(title = "", fixedrange = TRUE, range=y_axis_range)
    x <- list(title = "Time", fixedrange = TRUE)

    return(plotly::subplot(plot_stored, nrows = 1, shareY = TRUE, margin = 0.002) %>%
      plotly::config(displayModeBar = F) %>%
      plotly::layout(yaxis = y, xaxis = x, hovermode = "closest", legend = list(y = 1, orientation = "h")))

  } else {
    plot(x, session=session)
  }
}
