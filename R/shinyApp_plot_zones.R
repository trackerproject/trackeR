#' Plot training zones
#'
#' @param run_data An object of class \code{trackeRdata} as returned by \code{\link{readDirectory_shiny}}.
#' @param session A vector of selected sessions.
#' @param what A vector of variable names to be plotted.

plot_zones <- function(run_data, session, what = c("heart.rate")){
  x <- zones(run_data[session], what = what)
  # x <- runZones

  dat <- do.call("rbind", x)
  dat$zoneF <- factor(paste0("[", paste(dat$lower, dat$upper, sep = "-"), ")"),
                      levels = unique(paste0("[",paste(dat$lower, dat$upper,
                                                       sep = "-"), ")")),
                      ordered = TRUE)
  ## dat$session <- factor(dat$session)
  # dat$session <- sprintf("%02d", dat$session)

  dat$Session <- paste("Session", sprintf(paste0("%0", nchar(max(dat$session)), "d"), dat$session))
  dat$timeN <- as.numeric(dat$time)
  ## facets
  units <- getUnits(x)
  lab_data <- function(series) {
    thisunit <- units$unit[units$variable == series]
    prettyUnit <- prettifyUnits(thisunit)
    paste0(series, " [", prettyUnit, "]")
  }
  pal <-  colorFactor(c('deepskyblue', 'dodgerblue4'), dat$Session)

  individual_plots <- list()
  legend_status <- TRUE
  for (feature in what){
    y <- list(
      title = '% of time'
    )
    x <- list(
      title = paste0('Zones (', lab_data(feature), ')')
      # tickangle = 180
    )
    p <- plotly::plot_ly(subset(dat, variable == feature), x = ~zoneF, y = ~percent,
                         color = ~Session, colors = pal(dat$Session), legendgroup = ~Session) %>%
          plotly::add_bars() %>%
          plotly::layout(xaxis = x, yaxis = y, hovermode = 'closest')
    individual_plots[[feature]] <- style(p, showlegend = legend_status)
    legend_status <- FALSE
  }

  plots <- do.call(plotly::subplot, c(individual_plots, nrows = length(what),
                                      margin = 0.05, shareY = FALSE, titleX = TRUE, titleY = TRUE))

  return(plots)
}
