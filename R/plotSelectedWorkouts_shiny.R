plot_selectedWorkouts <- function(x, session, what, var_units, var_name_units){
  #plot(data, session = session, what = what)
  threshold = TRUE
  smooth = FALSE
  trend = TRUE
  dates = TRUE
  #x <- runs
  #session <- c(1:27)
  #what <-  c("pace", "heart.rate", 'altitude')
  
  Series <- NULL
  
  ## code inspired by autoplot.zoo
  if (is.null(session)) session <- seq_along(x)
  units <- getUnits(x)
  
  x <- x[session]
  
  ## threshold
  if (threshold){
    dots <- list()
    if (all(c("variable", "lower", "upper") %in% names(dots))){
      ## thresholds provided by user
      th <- data.frame(variable = dots$variable, lower = dots$lower, upper = dots$upper)
    } else {
      ## default thresholds
      cycling <- units$unit[units$variable == "cadence"] == "rev_per_min"
      th <- generateDefaultThresholds(cycling)
      ## th <- th[which(th$variable %in% what),]
      ## w <- which(units$variable %in% what)
      th <- changeUnits(th, variable = units$variable, unit = units$unit)
    }
    ## apply thresholds
    x <- threshold(x, th)
  }
  
  ## for plotting pace, always apply a threshold
  ## upper threshold is based on preferred walking speed of 1.4 m/s,
  ## see https://en.wikipedia.org/wiki/Preferred_walking_speed
  if ("pace" %in% what){
    conversionPace <- match.fun(paste("s_per_m", units$unit[units$variable == "pace"], sep = "2"))
    thPace <- conversionPace(1 / 1.4)
    x <- threshold(x, variable = "pace", lower = 0, upper = thPace)
  }
  
  ## smooth
  if (smooth) {
    xo <- x
    if (is.null(getOperations(x)$smooth)) {
      x <- smoother(x, what = what, ...)
    } else {
      warning("This object has already been smoothed. No additional smoothing takes place.")
      smooth <- FALSE ## it's not the plot function calling smoother
      x <- x
    }
  } else {
    x <- x
  }
  
  ## get data
  df <- if (smooth) fortify(xo, melt = TRUE) else fortify(x, melt = TRUE)
  
  ## prepare session id for panel header
  if (dates) {
    df$SessionID <- format(session[df$SessionID])
    df$SessionID <- gsub(" ", "0", df$SessionID)
    df$SessionID <- paste(df$SessionID, format(df$Index, "%Y-%m-%d"), sep = ": ")
  } else {
    df$SessionID <- factor(df$SessionID, levels = seq_along(session), labels = session)
  }
  df <- subset(df, Series %in% what)
  df$Series <- factor(df$Series)
  
  ## check that there is data to plot
  for(l in levels(df$Series)){
    if (all(is.na(subset(df, Series == l, select = "Value"))))
      df <- df[!(df$Series == l), ]
  }
  
  #df$Index1 <- as.Date(df$Index, format = "%Y-%m-%d  %H:%M:%S")
  
  #format(dataSelected$sessionStart, format = "%Y-%m-%d  %H:%M:%S")
  df$Series <- as.character(df$Series)
  df$id <- as.integer(factor(df$SessionID))
  df$numericDate <- as.numeric(df$Index)
  N = nlevels(factor(df$id))

  plot_stored = vector("list", N)
  
  

  for(i in unique(df$id)){
    smoothed_model <- gam(Value ~ s(numericDate, bs = 'cs'), data = df[(df$id==i),])
    smoothed_data <- predict(smoothed_model, newdata=df[(df$id==i),])
    a <- plot_ly(df[(df$id==i),], x = ~Index, y = ~Value, hoverinfo='none', alpha = 0.1, color = I('black')) %>%
       add_lines(showlegend = FALSE) %>% 
       add_lines(x = ~Index, y = smoothed_data, hoverinfo='text', text = ~paste(round(Value, 2), var_units),
                              color = I('deepskyblue3'),
                              showlegend = FALSE, alpha = 1)
    
    plot_stored[[i]] <- a
  }
  
  y <- list(
    title = var_name_units,
    fixedrange = TRUE
  )
  x <- list(
    title = 'Time',
    fixedrange = TRUE
  )
  return(subplot(plot_stored, nrows = 1, shareY = TRUE, margin = 0.002) %>% config(displayModeBar = F) %>%
           layout(showlegend = FALSE, yaxis = y, xaxis = x, hovermode = 'closest'))
  

}