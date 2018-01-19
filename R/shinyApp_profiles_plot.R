## Plot concentration profiles for given variables.
##
## @param run_data An object of class \code{trackeRdata}..
## @param session A vector of selected sessions.
## @param what A vector of variable names to be plotted.


plot_profiles <- function(run_data, session, what = c("speed")){

  # Get grid
  breaks <- list()

  df <- fortify(run_data)

  find_step_size <- function (maximum, minimum = 0) {
    value_range <- as.character(ceiling(maximum - minimum))
    range_size <- nchar(value_range)
    round_table <- list('1' = 5, '2' = 5, '3' = 10, '4' = 100,
                        '5' = 10000, '6' = 100000)
    maximum <- ceiling(maximum/round_table[[range_size]]) * round_table[[range_size]]
    step_size <- (maximum - minimum) / 500
    break_points <- seq(minimum, maximum, by = step_size)
    return(break_points)
  }

    for (feature in what) {
            if (all(is.na(df[[feature]]))) {
                warning(paste('No data for', feature))
                what <- what[!(what %in% feature)]
            }
    }

  for(feature in what) {
    maximum <- ceiling(quantile(df[feature], 0.99, na.rm = TRUE))
    minimum <- if (feature == 'heart.rate') 35 else 0
    breaks[[feature]] <- find_step_size(maximum, minimum)
  }

  # Generate distribution profile
  dProfile <- distributionProfile(run_data, session = session, what = what,
                                  grid = breaks)

  # Generate concentration profile
  cProfile <- concentrationProfile(dProfile, what = what)
  x <- cProfile

  session = NULL
  smooth = TRUE
  units <- getUnits(x)
  operations <- getOperations(x)

  ## select variables
  what <- what[what %in% names(x)]
  x <- x[what]  ## FIXME: implement [] method for profiles/variables instead of sessions
  class(x) <- "conProfile"
  attr(x, "operations") <- operations
  attr(x, "unit") <- units

  ## select sessions if (is.null(session)) { session <- attr(x[[1]], 'dimnames')[[2]]
  ## #1:ncol(x[[1]]) } else { if(is.numeric(session)) session <- attr(x[[1]],
  ## 'dimnames')[[2]][session] }
  availSessions <- if (is.null(ncol(x[[1]])))
    1 else ncol(x[[1]])
  if (is.null(session))
    session <- 1:availSessions
  for (i in what) x[[i]] <- x[[i]][, session]

  ## smooth
  if (smooth) {
    if (!is.null(operations$smooth)) {
      warning("This object has already been smoothed. No additional smoothing takes place.")
    } else {
      x <- smoother(x, what = what)
    }
  }

  ## get data
  rownames(x) <- NULL
  df <- fortify(x, melt = TRUE)

    if (length(session) < 2) {
        df$Series <- session  ## paste0('Session', session)
        ## df$Series <- factor(df$Series)
    }
    else {
        df$Series <- as.numeric(sapply(strsplit(as.character(df$Series), "Session"), function(x) x[2]))
    }
    df$Profile <- factor(df$Profile)

  ## ## check that there is data to plot for(l in levels(df$Series)){ if
  ## (all(is.na(subset(df, Series == l, select = 'Value')))) df <- df[!(df$Series == l), ]
  ## }

  ## make basic plot and facets
  singleVariable <- nlevels(df$Profile) == 1L
  singleSession <- nlevels(df$Series) == 1L
  lab_data <- function(series) {
    thisunit <- units$unit[units$variable == series]
    prettyUnit <- prettifyUnits(thisunit)
    paste0(series, " [", prettyUnit, "]")
  }

  df$series <- paste("Session", sprintf(paste0("%0", nchar(max(df$Series)), "d"), df$Series))

  # df$Series <- paste('Session', df$Series)
  #df$Series <- as.factor(df$Series)

  pal <-  leaflet::colorFactor(c('deepskyblue', 'dodgerblue4'), df$series)

  individual_plots <- list()
  legend_status <- TRUE

  for (feature in what){

    y <- list(title = 'dtime')
    x <- list(title = lab_data(feature))
    p <- plotly::plot_ly(df[df$Profile == feature, ], x = ~ Index, y = ~ Value,
                         color = ~series, colors = pal(df$series), legendgroup = ~series) %>%
        plotly::add_lines() %>%
        plotly::layout(xaxis = x, yaxis = y, hovermode = 'closest')
    individual_plots[[feature]] <- plotly::style(p, showlegend = legend_status)
    legend_status <- FALSE
  }

  plots <- do.call(plotly::subplot, c(individual_plots, nrows = length(what),
                                      margin = 0.05, shareY = FALSE, titleX = TRUE, titleY = TRUE))

  return(plots)
}





