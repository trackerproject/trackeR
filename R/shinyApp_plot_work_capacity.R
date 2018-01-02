#' Plot the work capacity W' (w prime).
#'
#' @param run_data An object of class \code{trackeRdata}.
#' @param session A vector of selected sessions.

plot_work_capacity <- function(run_data, session){
  quantity = "expended"
  cp = 4
  version = "2012"
  dates = TRUE
  scaled = TRUE

	x <- Wprime(object =run_data, session = session, quantity = quantity,
	     		cp = cp, version = version)

	# Automatically plot all sessions
	  session = NULL
	  dates = TRUE
	  scaled = TRUE


	Series <- NULL

	quantity <- attr(x, "quantity")
	cp <- attr(x, "cp")
	cycling <- attr(x, "cycling")
	Wunit <- if (cycling) "[J]" else "[m]"
	mylabels <- c(paste0(ifelse(cycling, "Power", "Speed"), " [", prettifyUnits(attr(x,
	                                                                                 "unit")$unit), "]"), paste("W'", quantity, "[scaled]"))

	## select sessions
	if (is.null(session)) session <- seq_along(x)
	x <- x[session]

	## transform W' to match power/speed scale
	if (scaled) {
	  sdMov <- stats::sd(unlist(lapply(x, function(z) z$movement)), na.rm = TRUE)
	  mMov <- mean(unlist(lapply(x, function(z) z$movement)), na.rm = TRUE)

	  x <- lapply(x, function(z) {
	    w <- (coredata(z$wprime) - mean(coredata(z$wprime), na.rm = TRUE))/stats::sd(coredata(z$wprime),
	                                                                                 na.rm = TRUE)
	    w <- w * sdMov  #sd(coredata(z$movement), na.rm = TRUE)
	    z$wprime <- w + mMov
	    # max(mMov, abs(min(w, na.rm = TRUE))) max(mean(coredata(z$movement), na.rm = TRUE),
	    # abs(min(w, na.rm = TRUE)))
	    return(z)
	  })
	}

	## get data
	class(x) <- "trackeRWprime"
	df <- fortify(x, melt = TRUE)

	## prepare session id for panel header
	if (dates) {
	  df$SessionID <- format(session[df$SessionID])
	  df$SessionID <- gsub(" ", "0", df$SessionID)
	  df$SessionID <- paste(df$SessionID, format(df$Index, "%Y-%m-%d"), sep = ": ")
	} else {
	  df$SessionID <- factor(df$SessionID, levels = seq_along(session), labels = session)
	}
	df$Series <- factor(df$Series)

	## check that there is data to plot
	for (l in levels(df$Series)) {
	  if (all(is.na(subset(df, Series == l, select = "Value"))))
	    df <- df[!(df$Series == l), ]
	}


	df$Series <- as.factor(as.character(df$Series))
	df$id <- as.integer(factor(df$SessionID))
	df$numericDate <- as.numeric(df$Index)
	N = nlevels(factor(df$id))

	plot_stored = vector("list", N)

  show_legend = T
  for(i in unique(df$id)){
    # smoothed_model <- gam(Value ~ s(numericDate, bs = 'cs'), data = df[(df$id==i),])
    # smoothed_data <- predict(smoothed_model, newdata=df[(df$id==i),])
    print(i)
    print(subset(df, (id == i) & (Series == 'movement')))
    a <- plot_ly(subset(df, (id == i) & (Series == 'movement')), x = ~Index, y = ~Value, hoverinfo='none',
                 color = I('gray'),legendgroup = ~Series,
                 name = mylabels[1], showlegend = show_legend) %>% add_lines(alpha=0.4) %>%
      add_lines(data=subset(df, (id == i) & (Series == 'wprime')), x = ~Index, y = ~Value, hoverinfo='none',
                color = I('#337ab7'), legendgroup = ~Series, name = mylabels[2], showlegend = show_legend)

    plot_stored[[i]] <- a
    show_legend = F
  }

  plot_stored <- plot_stored[!sapply(plot_stored, is.null)]

  y <- list(
    title = '',
    fixedrange = TRUE
  )
  x <- list(
    title = 'Time',
    fixedrange = TRUE
  )

  return(subplot(plot_stored, nrows = 1, shareY = TRUE, margin = 0.002) %>% config(displayModeBar = F) %>%
           layout(yaxis = y, xaxis = x, hovermode = 'closest', legend = list(y = 1, orientation = 'h')))

}

