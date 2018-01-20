## Plot the profile for each selected session for a given variable (heart rate, altitude, pace).
##
## @param x An object of class \code{trackeRdata}.
## @param session A vector of selected sessions.
## @param what A character of the variable to be plotted (e.g. "heart.rate").
## @param var_units A character of the unit of measurement for the given variable (e.g. "bmp") generated using \code{lab_sum()}.
## @param var_name_units A character of the named unit of measurement for the given variable (e.g. "Heart Rate [bpm]") generated using \code{lab_sum()}.
## @param other arguments to be passed to the \code{control} arguments in \code{\link{smoother.trackeRdata}}

plot_selectedWorkouts <- function(x, session, what, var_units, var_name_units, ...){
    threshold <- TRUE
    smooth <- FALSE
    dates <- TRUE

    if (is.null(session)) {
        session <- seq_along(x)
    }
    units <- getUnits(x)
    x <- x[session]

    ## threshold
    if (threshold){
        dots <- list()
        if (all(c("variable", "lower", "upper") %in% names(dots))){
            ## thresholds provided by user
            th <- data.frame(variable = dots$variable, lower = dots$lower, upper = dots$upper)
        }
        else {
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

    if (smooth) {
        x <- smoother(x, ...)
    }

    df <- fortify(x, melt = TRUE)

    df$id <- session[df$SessionID]
    ## prepare session id for panel header
    if (dates) {
        df$SessionID <- format(df$id)
        df$SessionID <- gsub(" ", "0", df$SessionID)
        df$SessionID <- paste(df$SessionID, format(df$Index, "%Y-%m-%d"), sep = ": ")
    }
    else {
        df$SessionID <- factor(df$SessionID, levels = seq_along(session), labels = session)
    }
    df <- df[df$Series %in% what, ]

    df$Series <- as.character(df$Series)
    df$numericDate <- as.numeric(df$Index)
    N <- length(unique(df$id))

    ranges <- NULL
    for (i in unique(df$id)) {
        df_subset <- df[df$id == i,]
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
    }
    else {
        maximal_range <- c(min(ranges[, 1], na.rm = TRUE), max(ranges[, 2], na.rm = TRUE))
    }

    plot_stored <- list()
    for (i in unique(df$id)) {
        df_subset <- df[df$id == i,]
        has_values <- !all(is.na(df_subset[df_subset$Series == what, "Value"]))
        annotations_list <- list(
            text = paste('Session:', i),
            xref = "paper",
            yref = "paper",
            yanchor = "bottom",
            xanchor = "center",
            align = "center",
            x = 0.5,
            y = 1,
            showarrow = FALSE)
        axis_list <- list(zeroline = FALSE)
        if (has_values) {
            smoothed_model <- try(mgcv::gam(Value ~ s(numericDate, bs = 'cs'), data = df_subset), silent = TRUE)
            smoothed_data <- mgcv::predict.gam(smoothed_model, newdata = df_subset)
            a <- plotly::plot_ly(df_subset, x = ~ Index, y = ~ Value, hoverinfo = 'none',
                                 type = "scatter", mode = "lines",
                                 showlegend = FALSE, alpha = 0.1, color = I('black')) %>%
                plotly::add_lines(x = ~ Index, y = smoothed_data, hoverinfo = 'text',
                                  text = ~ paste(round(Value, 2), var_units),
                                  color = I('deepskyblue3'),
                                  showlegend = FALSE, alpha = 1) %>%
                plotly::layout(annotations = annotations_list,
                               xaxis = axis_list, yaxis = c(axis_list, list(range = maximal_range * 1.02)))
        }
        else {
            df_subset$Value <- if (na_ranges) 0 else mean(maximal_range)
            a <- plotly::plot_ly(df_subset, x = ~ Index, y = ~ Value, hoverinfo = 'none',
                                 type = "scatter", mode = "none",
                                 showlegend = FALSE) %>%
                plotly::layout(annotations = annotations_list,
                               xaxis = axis_list, yaxis = c(axis_list,
                                                            list(range = maximal_range * 1.02,
                                                                 showticklabels = FALSE)))
        }
        plot_stored[[as.character(i)]] <- a
    }

    y <- list(title = var_name_units, fixedrange = TRUE)
    x <- list(title = 'Time', fixedrange = TRUE)

    return(plotly::subplot(plot_stored, nrows = 1, shareY = FALSE, margin = 0.002)  %>%
           plotly::config(displayModeBar = FALSE) %>%
           plotly::layout(showlegend = FALSE, yaxis = y, xaxis = x, hovermode = 'closest'))
}
