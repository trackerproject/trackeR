#' A timeline plot for workouts.
#'
#' @param sumX An object of class \code{trackeRdataSummary}.
#' @param plotly Logical. Return plotly plots or standard trackeR plots
#' @param shiny Logical. Whether plots are in a shiny environment.

plot_timeline <- function(sumX, lims=NULL, shiny=TRUE, plotly=TRUE) {
  if (plotly) {
    d <- if(shiny) plotly::event_data("plotly_selected") else NULL
    startdates <- as.POSIXct(as.Date(sumX$sessionStart))
    enddates <- as.POSIXct(as.Date(sumX$sessionEnd))
    ## Hack to extract times
    endtimes <- sumX$sessionEnd
    starttimes <- sumX$sessionStart
    endtimes <- as.POSIXct(as.numeric(difftime(endtimes, trunc(endtimes, "days"), units = "secs")),
                           origin = Sys.Date())
    starttimes <- as.POSIXct(as.numeric(difftime(starttimes, trunc(starttimes, "days"),
                                                 units = "secs")), origin = Sys.Date())
    df <- data.frame(sday = startdates, eday = enddates, start = starttimes, end = endtimes, session=sumX$session)
    if (!is.null(lims)) {
      lims <- as.POSIXct(paste(Sys.Date(), lims))
    }

    key <- df$session
    p <- ggplot2::ggplot(df, ggplot2::aes(key=key)) +
      ggplot2::geom_point(ggplot2::aes(x = start, y = sday), alpha=0) +
      ggplot2::geom_segment(ggplot2::aes_(x = quote(start), xend = quote(end), y = quote(sday),
                                          yend = quote(eday),
                                          text=sprintf("Session: %s<br>Start: %s <br>End: %s",
                                                       df$session, sumX$sessionStart, sumX$sessionEnd)), color = '#428bca', size=1)
    # does not currently work
    # if (shiny){
    #   if (length(d[["key"]]) > 0) {
    #     m <- df[df$session %in% d[["key"]], ]

    #     for(i in c(1:nrow(m))) {
    #       p <- p + ggplot2::geom_segment(aes(x = m[i,'start'], y = m[i,'sday'], xend = m[i,'end'], yend = m[i,'eday']),
    #                                      color = "darkorange3", size=2)
    #     }

    #   }
    # }
    date_breaks <- if((max(startdates) - min(startdates) > 15)) round((max(startdates) - min(startdates))/15, 0) else 1
    ## take care of breaks, limits on the time axes and style of breakpoints
    p <- p + ggplot2::scale_x_datetime(date_labels = "%H", date_breaks = "1 hour")
    p <- p + ggplot2::scale_y_datetime(date_labels = "%d %b", date_breaks = paste(date_breaks, 'days'))
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 50, hjust = 1)) +
          ggplot2::xlab("Time") + ggplot2::ylab("")
    p <- p + ggplot2::theme_bw()
    p <- plotly::ggplotly(p, tooltip = c("text"))
    p <- plotly::layout(p, dragmode = "select")
    p
  }
  else {
    timeline(sumX)
  }

}

