#' Plot an object of class trackeRdataSummary.
#'
#' @param sumX An object of class \code{trackeRdataSummary}.
#' @param what Name of variables which should be plotted. Default is all.
#' @param date Should the date or the session number be used on the abscissa?
#' @param group Which group of variables should be plotted? This can either be
#'     \code{total} or \code{moving}. Default is both.
#' @param lines Should interpolating lines be plotted?
#' @param plotly Logical. Return plotly plots or standard TrackeR plots
#' @param shiny Logical. Whether plots are in a shiny environment.
#' @param ... Currently not used.
#' @seealso \code{\link{summary.trackeRdata}}

plot_workouts <- function(sumX, what, plotly=TRUE, shiny=TRUE, date = TRUE, group = c("total"), lines = TRUE) {

  feature <- lab_sum(feature = what, data = sumX)
  units_text <- lab_sum(feature = what, data = sumX, whole_text = FALSE)

  if (plotly) {
    ## the following line is just intended to prevent R CMD check to produce the NOTE 'no
    ## visible binding for global variable *' because those variables are used in subset()
    variable <- type <- NULL

    nsessions <- length(unique(sumX$session))
    ndates <- length(unique(sumX$sessionStart))
    units <- getUnits(sumX)

    ## subsets on variables and type
    dat <- fortify(sumX, melt = TRUE)
    if (!is.null(what)) {
        dat <- subset(dat, variable %in% what)
    }
    if (!is.null(group)) {
        dat <- subset(dat, type %in% group)
    }

    ## remove empty factor levels
    dat$variable <- factor(dat$variable)
    # dat$type <- factor(dat$type)

    ## clean up: if there are only NA observations for a variable, the (free) y-scale cannot
    ## be determined
    empty <- tapply(dat$value, dat$variable, function(x) all(is.na(x)))
    if (any(empty)) dat <- subset(dat, !(variable %in% names(empty)[empty]))

    ## single session
    if (nsessions < 2) {
        dat$sessionStart <- format(dat$sessionStart, format = "%Y-%m-%d")
        dat$session <- factor(dat$session)
    }

    ## x axis
    if (date) {
        dat$xaxis <- dat$sessionStart
        xlab <- "Date"
    } else {
        dat$xaxis <- dat$session
        xlab <- "Session"
    }

    d <- if(shiny) plotly::event_data("plotly_selected") else NULL
    what <- switch(what,
      "distance" = "Distance",
      "duration" = "Duration",
      "avgSpeed" = "Average Speed",
      "avgPace" = "Average Pace",
      "avgCadence" = "Average Cadence",
      "avgPower" = "Average Power",
      "avgHeartRate" = "Average Heart Rate",
      "wrRatio" = "work-to-rest ratio"
    )
    # print(d)
    p <- plotly::plot_ly(
      dat, x = ~xaxis, y = ~value, hoverinfo = "text",
      text = ~paste(
        "Date:", format(sessionStart, format = "%Y-%m-%d"),
        "\n", what, ":", round(value, 2), units_text
      )
    ) %>%
      plotly::add_markers(key = dat$session, color = I("deepskyblue3")) %>%
      plotly::add_lines(color = I("deepskyblue2"))
    if (shiny){
      if (length(d[["key"]]) > 0) {
        m <- dat[dat$session %in% d[["key"]], ]
        p <- plotly::add_markers(p, data = m, color = I("darkorange3"))
          # plotly::add_lines(data = m, color = I("darkorange2"))
      }
    }

    y <- list(title = feature)
    x <- list(title = "Date")

    plotly::layout(p, dragmode = "select", showlegend = FALSE, yaxis = y, xaxis = x, margin = list(l = 80, b = 50, pad = 0))
  } else {
    plot(sumX, what = what)
  }
}
