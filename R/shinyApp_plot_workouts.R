#' Plot an object of class trackeRdataSummary.
#'
#' @param sumX An object of class \code{trackeRdataSummary}.
#' @param what Name of variables which should be plotted. Default is all.
#' @param date Should the date or the session number be used on the abscissa?
#' @param group Which group of variables should be plotted? This can either be
#'     \code{total} or \code{moving}. Default is both.
#' @param lines Should interpolating lines be plotted?
#' @param shiny Logical. Whether plots are in a shiny environment.
#' @param sessions A vector. Selected sessions by session number.
#' @param sports A vector of sports of the sessions to be plotted.
#' @param ... Currently not used.
#' @seealso \code{\link{summary.trackeRdata}}

plot_workouts <- function(sumX, what, sessions, shiny = TRUE, date = TRUE,
                          group = c("total"), lines = TRUE, sports) {
  if (what != "wrRatio") {
    feature <- lab_sum(feature = what, data = sumX)
    units_text <- lab_sum(feature = what, data = sumX, whole_text = FALSE)
  } else {
    feature <- "Work-to-rest ratio"
    units_text <- NULL
  }
  ##  ............................................................................
  ##  Copied from core trackeR                                                ####
  ## the following line is just intended to prevent R CMD check to produce the NOTE 'no
  ## visible binding for global variable *' because those variables are used in subset()
  variable <- type <- NULL

  nsessions <- length(unique(sumX$session))
  ndates <- length(unique(sumX$sessionStart))
  units <- get_units(sumX)

  ## subsets on variables and type
  dat <- fortify(sumX, melt = TRUE)
  dat$sport <- sports
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


  ##  ............................................................................
  ##  Unique trackeR dashboard code                                           ####

  # d <- if(shiny) plotly::event_data("plotly_selected") else NULL

  # print(d)
  p <- plotly::plot_ly(
    dat,
    x = ~ xaxis, y = ~ value, hoverinfo = "text",
    text = ~ paste(
      "Session:", session, "\n",
      "Date:", format(sessionStart, format = "%Y-%m-%d"),
      "\n", convert_to_name(what), ":", round(value, 2), units_text, "\n",
      "Sport:", sport
    ), showlegend = FALSE
  ) %>%
    plotly::add_markers(
      key = dat$session, color = I("deepskyblue3"), symbol = ~ sport,
      symbols = c("circle", "x", "square"), size = I(8), legendgroup = ~ sport,
      showlegend = TRUE
    ) %>%
    plotly::add_lines(
      color = I("deepskyblue3"), connectgaps = TRUE, legendgroup = ~ sport,
      line = list(shape = "spline", smoothing = 0.5, showlegend = FALSE)
    )
  if (shiny) {
    if (length(unique(sessions)) != nsessions) {
      m <- as.data.frame(dat[dat$session %in% unique(sessions), ])
      # FIX for some reason cant plot when only 2 sessions selected
      if (nrow(m) == 2) {
        m <- rbind(m, m)
      }
      p <- plotly::add_markers(p,
        data = m, color = I("darkorange3"),
        size = I(9), symbol = ~ sport,
        symbols = c("circle", "x", "square"),
        showlegend = FALSE
      )
      # plotly::add_paths(data = m, color = I("darkorange3"))
    }
  }

  ra <- c(min(dat$xaxis), max(dat$xaxis))
  if(nsessions > 1) {
    ra[2] <- ra[2] + 0.01 * diff(ra)
    ra[1] <- ra[1] - 0.01 * diff(ra)
  }
  y <- list(title = feature, range = c(0, max(dat$value, na.rm = TRUE) * 2), fixedrange = TRUE)
  x <- list(title = "Date", fixedrange = TRUE, range = ra)

  plotly::layout(p,
    dragmode = "select", showlegend = TRUE, yaxis = y, legend = list(y = 1.1, orientation = "h"),
    xaxis = x, margin = list(l = 80, b = 50, pad = 0)
  )
}
