plot_zones <- function(x, session, what = c("speed"), breaks = c(0, 2:6, 12.5)){
  
  runZones <- zones(x[session], what = what, breaks = breaks)
  x <- runZones
  
  dat <- do.call("rbind", x)
  dat$zoneF <- factor(paste0("[", paste(dat$lower, dat$upper, sep = "-"), ")"),
                      levels = unique(paste0("[",paste(dat$lower, dat$upper, sep = "-"), ")")),
                      ordered = TRUE)
  ## dat$session <- factor(dat$session)
  dat$Session <- factor(paste('Session', dat$session))  ## rename for legend title
  
  dat$timeN <- as.numeric(dat$time)
  ## facets
  units <- getUnits(x)
  lab_data <- function(series) {
    thisunit <- units$unit[units$variable == series]
    prettyUnit <- prettifyUnits(thisunit)
    paste0(series, " [", prettyUnit, "]")
  }
  pal <-  colorFactor(c('deepskyblue', 'dodgerblue4'), dat$Session)
  
  y <- list(
    title = 'Percent of time per workout (%)'
  )
  x <- list(
    title = paste0('Zones (', lab_data('speed'), ')')
    # tickangle = 180
  )
  
  return(plot_ly(dat, x = ~zoneF, y = ~percent, color = ~Session, colors = pal(dat$Session)) %>%
    add_bars()  %>%
    layout(yaxis = y, xaxis = x, hovermode = 'closest')) 
}
