#' Plot the profile for each selected session for a given variable (heart rate, altitude, pace).
#'
#' @param x An object of class \code{\link{trackeRdata}}.
#' @param session A numeric vector of the sessions to be plotted, defaults to all sessions.
#' @param sumX An object of class \code{trackeRdataSummary}.
#' @param what Which variables should be plotted?
#' @param threshold Logical. Should thresholds be applied?
#' @param smooth Logical. Should the data be smoothed?
#' @param trend Logical. Should a smooth trend be plotted?
#' @param dates Logical. Should the date of the session be used in the panel header?
#' @param changepoints Logical. Whether changepoints should be identified and plotted.
#' @param print_changepoints Logical. Whether or not to print changepoint values (when changepoints = TRUE).
#' @param n_changepoints A numeric. The threshold for the maximum number of changepoints to search for.

plot_selectedWorkouts <- function(x, session, what, sumX, threshold = TRUE, smooth = FALSE,
                                  trend = TRUE, dates = TRUE, changepoints = FALSE,
                                  n_changepoints = 6, print_changepoints = FALSE) {
  sports <- sport(x)[session]
  var_name_units <- lab_sum(
    feature = what, data = sumX,
    transform_feature = FALSE
  )

  var_units <- lab_sum(
    feature = what, data = sumX,
    whole_text = FALSE, transform_feature = FALSE
  )

  ## code inspired by autoplot.zoo
  if (is.null(session)) session <- seq_along(x)
  units <- getUnits(x)

  x <- x[session]
  # Smooth the data
  x <- smoother(x)
  ## threshold
  if (threshold) {
    dots <- list()
    if (all(c("variable", "lower", "upper") %in% names(dots))) {
      ## thresholds provided by user
      th <- data.frame(variable = dots$variable, lower = dots$lower, upper = dots$upper)
    } else {
      ## default thresholds
      cycling <- units$unit[units$variable == "cadence"] == "rev_per_min"
      th <- generateDefaultThresholds(cycling)
      ## th <- th[which(th$variable %in% what),]
      ## w <- which(units$variable %in% what)
      th <- changeUnits(th, variable = units$variable, unit = units$unit)
    }
    ## apply thresholds
    x <- threshold(x, th)
  }

  ## for plotting pace, always apply a threshold
  ## upper threshold is based on preferred walking speed of 1.4 m/s,
  ## see https://en.wikipedia.org/wiki/Preferred_walking_speed
  if ("pace" %in% what) {
    conversionPace <- match.fun(paste("s_per_m", units$unit[units$variable == "pace"], sep = "2"))
    thPace <- conversionPace(1 / 1.4)
    x <- threshold(x, variable = "pace", lower = 0, upper = thPace)
  }

  ## smooth
  if (smooth) {
    xo <- x
    if (is.null(getOperations(x)$smooth)) {
      x <- smoother(x, what = what)
    } else {
      warning("This object has already been smoothed. No additional smoothing takes place.")
      smooth <- FALSE ## it's not the plot function calling smoother
      x <- x
    }
  } else {
    x <- x
  }

  ## get data
  df <- if (smooth) fortify(xo, melt = TRUE) else fortify(x, melt = TRUE)

  df$id <- session[df$SessionID]
  ## prepare session id for panel header
  if (dates) {
    df$SessionID <- format(session[df$SessionID])
    df$SessionID <- gsub(" ", "0", df$SessionID)
    df$SessionID <- paste(df$SessionID, format(df$Index, "%Y-%m-%d"), sep = ": ")
  }
  else {
    df$SessionID <- factor(df$SessionID, levels = seq_along(session), labels = session)
  }
  df <- subset(df, Series %in% what)
  df$Series <- as.character(df$Series)
  df$numericDate <- as.numeric(df$Index)
  N <- length(unique(df$id))

  ranges <- NULL
  for (i in unique(df$id)) {
    df_subset <- df[df$id == i, ]
    values <- df_subset[df_subset$Series == what, "Value"]
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

  plot_stored <- list()
  images <- list()
  smoothed_values <- list(maximum = numeric(), minimum = numeric())
  n_plot <- 0
  shapes <- list()
  changepoint_y_values <- c()
  step_size <- 1 / length(unique(df$id))
  start <- 0
  for (i in unique(df$id)) {
    n_plot <- n_plot + 1
    df_subset <- df[df$id == i, ]
    has_values <- !all(is.na(df_subset[df_subset$Series == what, "Value"]))
    df_subset <- if (has_values) df_subset[!is.na(df_subset$Value), ] else df_subset

    annotations_list <- list(
      text = paste("Session:", i),
      xref = "paper",
      yref = "paper",
      yanchor = "bottom",
      xanchor = "center",
      align = "center",
      x = 0.5,
      y = 1,
      showarrow = FALSE
    )
    axis_list <- list(zeroline = FALSE, fixedrange = TRUE)
    if (has_values) {
      if (changepoints) {

        # m.binseg <- changepoint::cpt.mean(df_subset$Value, method = "BinSeg",
        #                      penalty = 'Manual', pen.value = '700 * log(n)',
        #                      minseglen = length(df_subset$Value) / 100, Q = n_changepoints)
        n_sessions <- length(df_subset$Value) - 5
        m.binseg <- changepoint::cpt.mean(df_subset$Value[6:n_sessions],
          method = "BinSeg",
          penalty = "BIC",
          minseglen = length(df_subset$Value) / 100, Q = n_changepoints
        )
        x_values <- c(1, changepoint::cpts(m.binseg) + 5, length(df_subset$Value))
        y_values <- changepoint::coef(m.binseg)$mean

        if (print_changepoints) {
          print(df_subset$Index[changepoint::cpts(m.binseg)])
          print(changepoint::coef(m.binseg))
        }
        # initiate a line shape object
        line <- list(
          type = "line",
          line = list(color = "darkred", dash = "dash"),
          xref = paste0("x", n_plot),
          yref = paste0("y", n_plot)
        )


        for (k in c(1:(length(x_values) - 1))) {
          line[["x0"]] <- df_subset$Index[x_values[k]]
          line[["x1"]] <- df_subset$Index[x_values[k + 1]]
          line[c("y0", "y1")] <- y_values[k]
          changepoint_y_values <- c(changepoint_y_values, y_values[k])
          shapes[[length(shapes) + 1]] <- line
        }
      }
      smoothed_model <- mgcv::gam(Value ~ s(numericDate, bs = "cs"), data = df_subset)
      smoothed_data <- mgcv::predict.gam(smoothed_model, newdata = df_subset)
      smoothed_values$minimum <- c(smoothed_values$minimum, min(smoothed_data))
      smoothed_values$maximum <- c(smoothed_values$maximum, max(smoothed_data))
      a <- plotly::plot_ly(
        df_subset,
        x = ~ Index, y = ~ Value, hoverinfo = "none",
        type = "scatter", mode = "lines",
        showlegend = FALSE, alpha = 0.1, color = I("black")
      ) %>%
        plotly::add_lines(
          x = ~ Index, y = smoothed_data, hoverinfo = "text",
          text = paste(round(smoothed_data, 2), var_units),
          color = I("deepskyblue3"),
          showlegend = FALSE, alpha = 1
        )
      a <- a %>% plotly::layout(
        annotations = annotations_list,
        xaxis = axis_list, yaxis = c(axis_list, list(range = c(0, max(smoothed_data) * 1.1)))
      )
      # xaxis = axis_list, yaxis = c(axis_list, list(range = maximal_range * 1.02))
    }
    else {
      df_subset$Value <- if (na_ranges) 0 else mean(maximal_range)
      a <- plotly::plot_ly(
        df_subset,
        x = ~ Index, y = ~ Value, hoverinfo = "none",
        type = "scatter", mode = "none",
        showlegend = FALSE
      ) %>%
        plotly::layout(
          annotations = annotations_list,
          xaxis = axis_list, yaxis = c(
            axis_list,
            list(
              range = maximal_range * 1.02,
              showticklabels = TRUE
            )
          )
        )
    }
    plot_stored[[as.character(i)]] <- a
    sport_image <- switch(sports[which(i == unique(df$id))],
      "running" = "running.png",
      "cycling" = "cycling.png",
      "swimming" = "swimming.png"
    )

    images[[which(i == unique(df$id))]] <- list(
      source = sport_image,
      xref = "paper",
      yref = "paper",
      x = start + step_size / 2,
      y = 1,
      sizex = 0.1,
      sizey = 0.1,
      opacity = 0.8
    )
    start <- start + step_size
  }
  y_axis_range <- if (what == "heart.rate") {
    c(80, 200)
  } else {
    c(
      0.5 * min(c(changepoint_y_values, smoothed_values$minimum)),
      max(c(changepoint_y_values, smoothed_values$maximum)) * 1.5
    )
  }
  y <- list(title = var_name_units, fixedrange = TRUE, range = y_axis_range)
  x <- list(title = "Time", fixedrange = TRUE)

  return(plotly::subplot(plot_stored, nrows = 1, shareY = TRUE, margin = 0.003) %>%
    plotly::config(displayModeBar = FALSE) %>%
    plotly::layout(showlegend = FALSE, yaxis = y, xaxis = x, images = images, hovermode = "x", shapes = shapes))
}
