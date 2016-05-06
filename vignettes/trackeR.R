### replication R code for
### "trackeR: Infrastructure for Running and Cycling
### Data from GPS-Enabled Tracking Devices in R"
library("trackeR")
#set.seed(403)


##################################
### Section 3 Import utilities ###
##################################

filepath <- system.file("extdata", "2013-06-08-090442.TCX", package = "trackeR")
## read raw data
runDF <- readTCX(file = filepath, timezone = "GMT")
## runDF is a data.frame with the following structure
str(runDF)


###################################
### Section 4 trackeRdata class ###
###################################

## turn the raw data in runDF into a trackeRdata object
runTr0 <- trackeRdata(runDF)
## alternatively, use convenience function
runTr1 <- readContainer(filepath, type = "tcx", timezone = "GMT")
identical(runTr0, runTr1)

## data for examples
data("run", package = "trackeR")
data("runs", package = "trackeR")


#################################
### Section 5.1 Visualisation ###
#################################

## plot evolution of heart rate and pace 
plot(runs, session = 1:3)

## plot route taken during session 4
plotRoute(runs, session = 4, zoom = 13, source = "osm")


####################################
### Section 5.2 Scalar summaries ###
####################################

## print summaries
summary(runs, session = 1:2)
runSummary <- summary(runs, session = 1)
print(runSummary, digits = 3)

## plot summaries
runSummaryFull <- summary(runs)
plot(runSummaryFull, group = c("total", "moving"),
    what = c("avgSpeed", "distance", "duration", "avgHeartRate"))


#################################
### Section 5.3 Time in zones ###
#################################

## specify variable and zone boundaries
runZones <- zones(runs[1:4], what = "speed", breaks = list(speed = c(0, 2:6, 12.5)))
## if breaks is a named list, argument 'what' can be left unspecified
runZones <- zones(runs[1:4], breaks = list(speed = c(0, 2:6, 12.5)))
## if only a single variable is to be evaluated, 'breaks' can also be a vector
runZones <- zones(runs[1:4], what = "speed", breaks = c(0, 2:6, 12.5))
plot(runZones)


#############################################
### Section 5.4 Quantifying work capacity ###
#############################################

wexp <- Wprime(runs, session = 11, quantity = "expended",
               cp = 4, version = "2012")
plot(wexp, scaled = TRUE)


###########################################################
### Section 5.5 Distribution and concentration profiles ###
###########################################################

## distribution profiles
dProfile <- distributionProfile(runs, session = 1:4,
  what = c("speed", "heart.rate"),
  grid = list(speed = seq(0, 12.5, by = 0.05), heart.rate = seq(0, 250)))
plot(dProfile, multiple = TRUE)

## concentration profiles
cProfile <- concentrationProfile(dProfile, what = "speed")
plot(cProfile, multiple = TRUE)


###############################################
### Section 6 Handling units of measurement ###
###############################################

## get the units for the variables in run
getUnits(run)

## change the unit of speed into miles per hour
runTr2 <- changeUnits(run, variable = "speed", unit = "mi_per_h")
getUnits(runTr2)

## use feet per hour as unit for speed
m_per_s2ft_per_h <- function(x) x * 3937/1200 * 3600
changeUnits(runSummary, variable = "speed", unit = "ft_per_h")


############################################
### Section 7 Thresholding and smoothing ###
############################################

## without thresholds
plot(runs, session = 4, what = "speed", threshold = FALSE)
## with default thresholds
plot(runs, session = 4, what = "speed") + ggplot2::expand_limits(y = c(0, 21))
## with default thresholds and smoothing
plot(runs, session = 4, what = "speed", smooth = TRUE, fun = "median", width = 20) +
  ggplot2::expand_limits(y = c(0, 12.5))
## thresholding and smoothing outside of plot method
run4 <- threshold(runs[4])
run4S <- smoother(run4, what = "speed", fun = "median", width = 20)
plot(run4S, what = "speed", smooth = FALSE) + ggplot2::expand_limits(y = c(0, 12.5))



############################
### Section 8 Case study ###
############################

library("trackeR")
## load data
data("runs", package = "trackeR")
## apply default thresholds
runsT <- threshold(runs)
## get and smooth distribution profiles 
dpRuns <- distributionProfile(runsT, what = "speed")
dpRunsS <- smoother(dpRuns)
## get concentration profiles
cpRuns <- concentrationProfile(dpRunsS)
plot(cpRuns, multiple = TRUE, smooth = FALSE) + ggplot2::theme(legend.position = "none")

## prepare functional data
library("fda")
gridSpeed <- seq(0, 12.5, length = 251)
sp <- matrix(unlist(cpRuns$speed), ncol = 250, byrow = TRUE,
  dimnames = list(names(cpRuns$speed), gridSpeed[-1]))
spfd <- Data2fd(argvals = gridSpeed[-1], y = t(sp))
## fit functional PCA
sppca <- pca.fd(spfd, nharm = 4)
## share of variance
varprop <- round(sppca$varprop * 100); names(varprop) <- 1:4
cumsum(varprop)

## plot harmonics
## plot function for pca objects adapted to include xlab as used in the vignette
mypcaplot <- function (x, nx = 128, pointplot = TRUE, harm = 0, expand = 0, 
    cycle = FALSE, xlab = "argvals", ...) 
{
    pcafd <- x
    if (!(inherits(pcafd, "pca.fd"))) 
        stop("Argument 'x' is not a pca.fd object.")
    harmfd <- pcafd[[1]]
    basisfd <- harmfd$basis
    rangex <- basisfd$rangeval
    if (length(nx) > 1) {
        argvals <- nx
        nx <- length(x)
    }
    else {
        argvals <- seq(rangex[1], rangex[2], length = nx)
    }
    fdmat <- eval.fd(argvals, harmfd)
    meanmat <- eval.fd(argvals, pcafd$meanfd)
    dimfd <- dim(fdmat)
    nharm <- dimfd[2]
    plotsPerPg <- sum(par("mfrow"))
    harm <- as.vector(harm)
    if (harm[1] == 0) 
        harm <- (1:nharm)
    if (length(dimfd) == 2) {
        for (jharm in 1:length(harm)) {
            if (jharm == 2) {
                op <- par(ask = TRUE)
                on.exit(par(op))
            }
            iharm <- harm[jharm]
            if (expand == 0) {
                fac <- sqrt(pcafd$values[iharm])
            }
            else {
                fac <- expand
            }
            vecharm <- fdmat[, iharm]
            pcmat <- cbind(meanmat + fac * vecharm, meanmat - 
                fac * vecharm)
            if (pointplot) 
                plottype <- "p"
            else plottype <- "l"
            percentvar <- round(100 * pcafd$varprop[iharm], 1)
            plot(argvals, meanmat, type = "l", ylim = c(min(pcmat), 
                max(pcmat)), ylab = paste("Harmonic", iharm), xlab = xlab,
                main = paste("PCA Function", iharm, "(Percentage of Variability", 
                  percentvar, ")"))
            if (pointplot) {
                points(argvals, pcmat[, 1], pch = "+")
                points(argvals, pcmat[, 2], pch = "-")
            }
            else {
                lines(argvals, pcmat[, 1], lty = 2)
                lines(argvals, pcmat[, 2], lty = 3)
            }
        }
    }
    else {
        if (cycle && dimfd[3] == 2) {
            meanmat <- drop(meanmat)
            for (jharm in 1:length(harm)) {
                if (jharm == 2) {
                  op <- par(ask = TRUE)
                  on.exit(par(op))
                }
                iharm <- harm[jharm]
                {
                  if (expand == 0) 
                    fac <- 2 * sqrt(pcafd$values[iharm])
                  else fac <- expand
                }
                matharm <- fdmat[, iharm, ]
                mat1 <- meanmat + fac * matharm
                mat2 <- meanmat - fac * matharm
                if (pointplot) 
                  plottype <- "p"
                else plottype <- "l"
                percentvar <- round(100 * pcafd$varprop[iharm], 
                  1)
                plot(meanmat[, 1], meanmat[, 2], type = plottype, 
                  xlim = c(min(c(mat1[, 1], mat2[, 1])), max(c(mat1[, 
                    1], mat2[, 1]))), ylim = c(min(c(mat1[, 2], 
                    mat2[, 2])), max(c(mat1[, 2], mat2[, 2]))), 
                  main = paste("PCA Function", iharm, "(Percentage of Variability", 
                    percentvar, ")"), ...)
                if (pointplot) {
                  points(mat1[, 1], mat1[, 2], pch = "+")
                  points(mat2[, 1], mat2[, 2], pch = "-")
                }
                else {
                  lines(mat1[, 1], mat1[, 2], lty = 2)
                  lines(mat2[, 1], mat2[, 2], lty = 3)
                }
            }
        }
        else {
            for (jharm in 1:length(harm)) {
                if (jharm == 2) {
                  op <- par(ask = TRUE)
                  on.exit(par(op))
                }
                iharm <- harm[jharm]
                fac <- {
                  if (expand == 0) 
                    sqrt(pcafd$values[iharm])
                  else expand
                }
                meanmat <- drop(meanmat)
                matharm <- fdmat[, iharm, ]
                nvar <- dim(matharm)[2]
                for (jvar in 1:nvar) {
                  pcmat <- cbind(meanmat[, jvar] + fac * matharm[, 
                    jvar], meanmat[, jvar] - fac * matharm[, 
                    jvar])
                  if (pointplot) 
                    plottype <- "p"
                  else plottype <- "l"
                  percentvar <- round(100 * pcafd$varprop[iharm], 
                    1)
                  plot(argvals, meanmat[, jvar], type = plottype, xlab = xlab,
                    ylab = paste("Harmonic", iharm), sub = paste("PCA Function", 
                      iharm, "(Percentage of Variability", percentvar, 
                      ")"), main = dimnames(fdmat)[[3]][jvar], 
                    ...)
                  if (pointplot) {
                    points(argvals, pcmat[, 1], pch = "+")
                    points(argvals, pcmat[, 2], pch = "-")
                  }
                  else {
                    lines(argvals, pcmat[, 1], lty = 2)
                    lines(argvals, pcmat[, 2], lty = 3)
                  }
                }
            }
        }
    }
    invisible(NULL)
}

par(mfrow = c(2,1), ask = FALSE)
mypcaplot(sppca, harm = 1:2, pointplot = FALSE, xlab = "Speed [m/s]") 
par(mfrow = c(1,1))


## plot scores vs summary statistics
runsSummary <- summary(runs)
scoresSP <- data.frame(sppca$scores)
names(scoresSP) <- paste0("speed_pc", 1:4)
d <- cbind(runsSummary, scoresSP)

library("ggplot2")
## pc1 ~ session duration (moving)
d$durationMoving <- as.numeric(d$durationMoving)
ggplot(d) + geom_point(aes(x = durationMoving, y = speed_pc1)) +
    theme_bw() + labs(x = "duration moving [min]", y = "PC1")
## pc2 ~ avg speed (moving)
ggplot(d) + geom_point(aes(x = avgSpeedMoving, y = speed_pc2)) +
    theme_bw() + labs(x = "average speed moving [m/s]", y = "PC2")


