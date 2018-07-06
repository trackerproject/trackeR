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
#' @param unit_reference_sport A character. The sport to be used as reference for units.
#' @param moving_threshold A numeric for the threshold.
#' @param desampling A numeric proportion between (0-1] for the proportion of raw data to be plotted.
plot_selectedWorkouts <- function(x, session, what, sumX, threshold = TRUE, smooth = FALSE,
                                  trend = TRUE, dates = TRUE, changepoints = FALSE,
                                  n_changepoints = 6, print_changepoints = FALSE,
                                  unit_reference_sport = NULL, moving_threshold = NULL,
                                  desampling = 1) {
  sports <- get_sport(x)[session]

  var_name_units <- lab_sum(
    feature = what, data = sumX,
    transform_feature = FALSE
  )

  var_units <- lab_sum(
    feature = what, data = sumX,
    whole_text = FALSE, transform_feature = FALSE
  )

  x <- x[session]

  ##  ............................................................................
  ##  Copied from core trackeR                                                ####
  units <- get_units(x)

  if (is.null(session)) {
    session <- seq_along(x)
  }

  if (is.null(unit_reference_sport)) {
    unit_reference_sport <- find_unit_reference_sport(x)
  }
  ## Match units to those of unit_reference_sport
  un <- collect_units(units, unit_reference_sport)
  for (va in unique(un$variable)) {
    units$unit[units$variable == va] <- un$unit[un$variable == va]
  }

  ## convert moving_threshold
  if (is.null(moving_threshold)) {
    moving_threshold <- c(cycling = 2, running = 1, swimming = 0.5)
    speed_unit <- un$unit[un$variable == "speed"]
    if (speed_unit != "m_per_s") {
      conversion <- match.fun(paste("m_per_s", speed_unit, sep = "2"))
      moving_threshold <- conversion(moving_threshold)
    }
  }
  ## Change units to those of unit_reference_sport
  x <- change_units(x, units$variable, units$unit, units$sport)

  ## threshold
  if (threshold) {
    dots <- list()
    if (all(c("variable", "lower", "upper", "sport") %in% names(dots))) {
      th <- generate_thresholds(dots$variable, dots$lower, dots$upper, dots$sport)
    } else {
      ## default thresholds
      th <- generate_thresholds()
      th <- change_units(th, variable = units$variable, unit = units$unit, sport = units$sport)
    }
    ## apply thresholds
    x <- threshold(x, th$variable, th$lower, th$upper, th$sport)
  }

  speed_unit <- strsplit(un$unit[un$variable == "speed"], split = "_per_")[[1]]
  pace_unit <- paste(speed_unit[2], speed_unit[1], sep = "_per_")
  convert_pace <- match.fun(paste(pace_unit, un$unit[un$variable == "pace"], sep = "2"))

  x <- threshold(x,
    variable = c("pace", "pace", "pace"),
    lower = c(0, 0, 0),
    upper = convert_pace(1 / moving_threshold),
    sport = names(moving_threshold)
  )

  ##  ............................................................................
  ##  trackeR dashboard unique code                                           ####
  var_name_units <- unique(var_name_units)
  plot_stored <- list()
  images <- list()
  smoothed_values <- list(maximum = numeric(), minimum = numeric())
  n_plot <- 0
  shapes <- list()
  changepoint_y_values <- c()
  step_size <- 1 / length(unique(session))
  start <- 0
  # Loop through each session
  for (i in session) {
    df_subset <- x[[which(i == session)]]
    dates <- index(df_subset)
    df_subset <- as.data.frame(df_subset)
    df_subset$Index <- dates
    df_subset$id <- i
    df_subset$SessionID <- i
    df_subset$SessionID <- paste0(paste(df_subset$SessionID, get_sport(x[which(i == session)]), 
                                        sep = ": "), 
                               "\n", format(df_subset$Index, "%Y-%m-%d"))
    df_subset$numericDate <- as.numeric(df_subset$Index)
   
    n_plot <- n_plot + 1
    colnames(df_subset)[which(colnames(df_subset) == what)] <- "Value"
    # df_subset <- df[df$id == i, ]
    has_values <- !all(is.na(df_subset[, "Value"]))
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
      sampled_rows <- sort(sample(index(df_subset), size = length(index(df_subset)) * desampling))
      a <- plotly::plot_ly(
        df_subset[sampled_rows, ],
        x = ~ Index, y = ~ Value, hoverinfo = "none",
        type = "scatter", mode = "lines",
        showlegend = FALSE, alpha = 0.1, color = I("black")
      ) %>%
        plotly::add_lines(
          data = df_subset,
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
      maximal_range <- c(-1, 1)
      df_subset$Value <- 0
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
    sport_image <- switch(sports[which(i == session)],
      "running" = "running.png",
      "cycling" = "cycling.png",
      "swimming" = "swimming.png"
    )

    images[[which(i == session)]] <- list(
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
    plotly::layout(
      showlegend = FALSE, yaxis = y, xaxis = x, images = images,
      hovermode = "x", shapes = shapes
    ))
}