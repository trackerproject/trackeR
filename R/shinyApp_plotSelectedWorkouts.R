#' Plot the profile for each selected session for a given variable (heart rate, altitude, pace).
#'
#' @param x An object of class \code{\link{trackeRdata}}.
#' @param session A numeric vector of the sessions to be plotted, defaults to all sessions.
#' @param sumX An object of class \code{\link{trackeRdataSummary}}.
#' @param what Which variables should be plotted?
#' @param threshold Logical. Should thresholds be applied?
#' @param smooth Logical. Should the data be smoothed?
#' @param trend Logical. Should a smooth trend be plotted?
#' @param dates Logical. Should the date of the session be used in the panel header?
#' @param plotly Logical. Whether return plotly plots or standard TrackeR plot.

plot_selectedWorkouts <- function(x, session, what, sumX, threshold = TRUE, smooth = FALSE, trend = TRUE, dates = TRUE,
                                  plotly=TRUE, ...) {

  if (plotly) {
    var_name_units <- lab_sum(feature = what, data = sumX,
                               transform_feature = FALSE)

    var_units <- lab_sum(feature = what, data = sumX,
                         whole_text = FALSE, transform_feature = FALSE)

    ## the following line is just intended to prevent R CMD check to produce the NOTE
    ## "no visible binding for global variable 'Series'" because that variable is used in subset()
    Series <- NULL

    ## code inspired by autoplot.zoo
    if (is.null(session)) session <- seq_along(x)
    units <- getUnits(x)

    x <- x[session]

    ## threshold
    if (threshold){
        dots <- list(...)
        if (all(c("variable", "lower", "upper") %in% names(dots))){
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
    if ("pace" %in% what){
        conversionPace <- match.fun(paste("s_per_m", units$unit[units$variable == "pace"], sep = "2"))
        thPace <- conversionPace(1 / 1.4)
        x <- threshold(x, variable = "pace", lower = 0, upper = thPace)
    }

    ## smooth
    if (smooth) {
        xo <- x
        if (is.null(getOperations(x)$smooth)) {
            x <- smoother(x, what = what, ...)
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
    smoothed_values <- list(maximum=numeric(), minimum=numeric())
    for (i in unique(df$id)) {
      df_subset <- df[df$id == i, ]
      has_values <- !all(is.na(df_subset[df_subset$Series == what, "Value"]))
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
        smoothed_model <- mgcv::gam(Value ~ s(numericDate, bs = "cs"), data = df_subset)

        smoothed_data <- mgcv::predict.gam(smoothed_model, newdata = df_subset)
        smoothed_values$minimum <- c(smoothed_values$minimum, min(smoothed_data))
        smoothed_values$maximum <- c(smoothed_values$maximum, max(smoothed_data))
        a <- plotly::plot_ly(
          df_subset, x = ~ Index, y = ~ Value, hoverinfo = "none",
          type = "scatter", mode = "lines",
          showlegend = FALSE, alpha = 0.1, color = I("black")
        ) %>%
          plotly::add_lines(
            x = ~ Index, y = smoothed_data, hoverinfo = "text",
            text = paste(round(smoothed_data, 2), var_units),
            color = I("deepskyblue3"),
            showlegend = FALSE, alpha = 1
          ) %>%
          plotly::layout(
            annotations = annotations_list,
            xaxis = axis_list, yaxis = c(axis_list, list(range = c(min(smoothed_data) * 0.9, max(smoothed_data) * 1.1)))
            # xaxis = axis_list, yaxis = c(axis_list, list(range = maximal_range * 1.02))
          )
      }
      else {
        df_subset$Value <- if (na_ranges) 0 else mean(maximal_range)
        a <- plotly::plot_ly(
          df_subset, x = ~ Index, y = ~ Value, hoverinfo = "none",
          type = "scatter", mode = "none",
          showlegend = FALSE
        ) %>%
          plotly::layout(
            annotations = annotations_list,
            xaxis = axis_list, yaxis = c(
              axis_list,
              list(
                range = maximal_range * 1.02,
                showticklabels = FALSE
              )
            )
          )
      }
      plot_stored[[as.character(i)]] <- a
    }
    y_axis_range <- if(what == 'heart.rate') {c(80, 200)} else {c(min(smoothed_values$minimum) * 0.9, max(smoothed_values$maximum) * 1.1)}
    y <- list(title = var_name_units, fixedrange = TRUE, range=y_axis_range)
    x <- list(title = "Time", fixedrange = TRUE)

    return(plotly::subplot(plot_stored, nrows = 1, shareY = TRUE, margin = 0.003) %>%
      plotly::config(displayModeBar = FALSE) %>%
      plotly::layout(showlegend = FALSE, yaxis = y, xaxis = x, hovermode = "closest"))
  } else {
    plot(x, session = session, what = what)
  }

}
