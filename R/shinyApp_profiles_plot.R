#' Plot concentration profiles for given variables.
#'
#' @param x An object of class \code{trackeRdata}.
#' @param session A vector of selected sessions.
#' @param what A vector of variable names to be plotted.
#' @param profiles_calculated Pre-calculated concentration profiles for all sessions.
plot_concentration_profiles <- function(x, session, profiles_calculated, 
                                        what = c("speed"), 
                                        smooth = TRUE, limits = NULL) {

  ## Generate distribution profile
  # dProfile <- distributionProfile(x, session = session, what = what, auto_grid = TRUE)
  tracker_object <- x
  ## Generate concentration profile
  x <- get_profile(object = profiles_calculated, session = session, what = what)
  # x <- concentration_profile(x, session = session, what = what, limits = limits)
  ## duration unit; sport does not matter here as units have been uniformised already
  units <- get_units(x)
  duration_unit <- units$unit[units$sport == "running" & units$variable == "duration"]
  ## fortify
  df <- fortify(x, melt = TRUE)
  df$Series <- as.numeric(sapply(strsplit(as.character(df$Series), "session"), function(x) x[2]))
  df$Profile <- factor(df$Profile)
  
  ## make basic plot and facets
  lab_data <- function(series) {
    thisunit <- units$unit[units$sport == "running" & units$variable == series]
    prettyUnit <- prettifyUnits(thisunit)
    paste0(series, " [", prettyUnit,"]")
  }
  df$series <- paste("Session", sprintf(paste0("%0", nchar(max(df$Series)), "d"), df$Series))
  pal <- leaflet::colorFactor(c("deepskyblue", "dodgerblue4"), df$series)

  individual_plots <- list()
  legend_status <- TRUE

  for (feature in what) {
    y <- list(title = "dtime")
    x <- list(title = lab_sum(feature, data = tracker_object, whole_text = TRUE, 
                              transform_feature = FALSE))
    feature_profile <- df[df$Profile == feature, ]
    feature_profile$Value[is.na(feature_profile$Value)] <- 0
    p <- plotly::plot_ly(
      feature_profile, x = ~ Index, y = ~ Value,
      color = ~series, colors = pal(feature_profile$series), legendgroup = ~Series,
      hoverinfo = "text", text = ~ round(Index, 1)
    ) %>%
      plotly::add_lines() %>%
      plotly::layout(xaxis = x, yaxis = y, hovermode = "closest")
    individual_plots[[feature]] <- plotly::style(p, showlegend = legend_status)
    legend_status <- FALSE
  }

  plots <- do.call(plotly::subplot, c(
    individual_plots, nrows = length(what),
    margin = 0.05, shareY = FALSE, titleX = TRUE, titleY = TRUE
  ))

  return(plots)
}
