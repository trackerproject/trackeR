## Plot concentration profiles for given variables.
##
## @param run_data An object of class \code{trackeRdata}..
## @param session A vector of selected sessions.
## @param what A vector of variable names to be plotted.


plot_profiles <- function(run_data, session, what = c("speed")){

    ## Generate distribution profile
    dProfile <- distributionProfile(run_data, session = session, what = what,
                                    auto_grid = TRUE)

    ## Generate concentration profile
    x <- concentrationProfile(dProfile, what = what)

    units <- getUnits(x)
    operations <- getOperations(x)

    ## select variables
    what <- what[what %in% names(x)]
    x <- x[what]  ## FIXME: implement [] method for profiles/variables instead of sessions
    class(x) <- "conProfile"
    attr(x, "operations") <- operations
    attr(x, "unit") <- units

    availSessions <- if (is.null(ncol(x[[1]])))
                         1 else ncol(x[[1]])

    if (is.null(session))
        session <- 1:availSessions

    for (i in what) x[[i]] <- x[[i]][, session]

    x <- smoother(x, what = what)

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

    pal <-  leaflet::colorFactor(c('deepskyblue', 'dodgerblue4'), df$series)

    individual_plots <- list()
    legend_status <- TRUE

    for (feature in what){
        y <- list(title = 'dtime')
        x <- list(title = lab_data(feature))
        feature_profile <- df[df$Profile == feature, ]
        feature_profile$Value[is.na(feature_profile$Value)] <- 0
        p <- plotly::plot_ly(feature_profile, x = ~ Index, y = ~ Value,
                             color = ~series, colors = pal(feature_profile$series), legendgroup = ~series) %>%
            plotly::add_lines() %>%
            plotly::layout(xaxis = x, yaxis = y, hovermode = 'closest')
        individual_plots[[feature]] <- plotly::style(p, showlegend = legend_status)
        legend_status <- FALSE
    }

    plots <- do.call(plotly::subplot, c(individual_plots, nrows = length(what),
                                        margin = 0.05, shareY = FALSE, titleX = TRUE, titleY = TRUE))

    return(plots)
}





