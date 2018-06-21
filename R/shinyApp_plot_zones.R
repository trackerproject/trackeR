#' Plot training zones.
#'
#' @param x An object of class \code{trackeRdata}.
#' @param session A vector of selected sessions.
#' @param what A vector of variable names to be plotted.

plot_zones <- function(x, session, what = c("heart_rate"), n_zones) {
  x <- zones(x, session = session, what = what, auto_breaks = TRUE, n_zones = n_zones)

  dat <- do.call("rbind", x)
  dat$zoneF <- factor(
    paste0("[", paste(dat$lower, dat$upper, sep = "-"), ")"),
    levels = unique(paste0("[", paste(
      dat$lower, dat$upper,
      sep = "-"
    ), ")")),
    ordered = TRUE
  )
  dat$Session <- paste("Session", sprintf(
    paste0("%0", nchar(max(dat$session)), "d"),
    dat$session
  ))
  dat$timeN <- as.numeric(dat$time)
  ## facets
  units <- getUnits(x)
  lab_data <- function(series) {
    thisunit <- units$unit[units$variable == series]
    prettyUnit <- prettifyUnits(thisunit)
    paste0(series, " [", prettyUnit, "]")
  }
  pal <- leaflet::colorFactor(c("deepskyblue", "dodgerblue4"), dat$Session)

  individual_plots <- list()
  legend_status <- TRUE
  for (feature in what) {
    y <- list(title = "% of time")
    x <- list(title = paste0("Zones (", lab_data(feature), ")"))
    feature_zones <- dat[dat$variable == feature, ]
    p <- plotly::plot_ly(
      feature_zones, x = ~ zoneF, y = ~ percent,
      color = ~Session, colors = pal(feature_zones$Session), legendgroup = ~ Session, hoverinfo = "text",
            text = ~ paste0(round(percent, 1), '%')
    ) %>%
      plotly::add_bars() %>%
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
