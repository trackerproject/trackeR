## Plot training zones
##
## @param run_data An object of class \code{trackeRdata} as returned by \code{\link{readDirectory}}.
## @param session A vector of selected sessions.
## @param what A vector of variable names to be plotted.

plot_zones <- function(run_data, session, what = c("heart.rate")) {
  x <- zones(run_data, session = session, what = what, auto_breaks = TRUE)
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
      color = ~Session, colors = pal(feature_zones$Session), legendgroup = ~ Session
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