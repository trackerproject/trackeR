# trackeR

### Description

The purpose of this package is to provide infrastructure for handling
running and cycling data from GPS-enabled tracking devices.

The formats that are currently supported for the training activity
files are .tcx (Training Center XML) and .db3. After extraction and
appropriate manipulation of the training or competition attributes,
the data are placed into session-based and unit-aware data objects of
class trackeRdata (S3 class). The information in the resultant data objects
can then be visualised, summarised, and analysed through corresponding
flexible and extensible methods.

### Current capabilities

Read:
- Read data from .tcx or .db3 files.
- Read all supported files in a specfied directory.

Data processing:
- Automatically identify sessions from timestamps.
- Imputation of data to characterize times when the device is paused or remains stationary.
- Correction of GPS-measured distances using elevation data.
- Basic data cleaning capabilities e.g., no negative speeds or distances.
- Specify and conveniently change units of measurement.
- Organise data into session-based and unit-aware data objects of class trackeRdata.

Analysis:
- Session summaries: distance, duration, time moving, average speed/pace/heart
rate/cadence/power (overall and moving), work to rest ratio.
- Time spent exercising in user-supplied zones, e.g., heart rate zones or speed zones.
- Work capacity above critical power (W', W prime)
- Distribution profiles: time spent exercising above thresholds of training attributes.
- Concentration profiles: negative derivatives of distribution profiles.

Visualisation:
- Plot session progression in, e.g., pace, heart rate, etc.
- Plot route covered during session on maps from various providers.
- Plot session summary statistics.
- Plot time spent exercising in zones.
- Plot distribution/concentration profiles.


### Installation

Install the development version from github:

```
# install.packages("devtools")
devtools::install_github("hfrick/trackeR")
```

### Example

Summarize sessions
```
library("trackeR")
data(runs, package = "trackeR")
runsSummary <- summary(runs)
plot(runsSummary, group = c("total", "moving"),
    what = c("avgSpeed", "distance", "duration", "avgHeartRate"))
```

Generate distribution and concentration profiles
```{r}
dpRuns <- distributionProfile(runs)
dpRunsS <- smoother(dpRuns)
cpRuns <- concentrationProfile(dpRunsS)
plot(cpRuns, multiple = TRUE, smooth = FALSE)
```

Explore concentration profiles for speed, e.g., via functional principal components analysis
```{r}
library("fda")
## prepare data
gridSpeed <- seq(0, 12.5, length = 251)
sp <- matrix(unlist(cpRuns$speed), ncol = 250, byrow = TRUE,
             dimnames = list(names(cpRuns$speed), gridSpeed[-1]))
spfd <- Data2fd(argvals = gridSpeed[-1], y = t(sp),
                fdnames = c("Speed", "Session", "d Time"))
		
## fit + select number of harmonics
sppca <- pca.fd(spfd, nharm = 4)
varprop <- sppca$varprop * 100
names(varprop) <- 1:4
cumsum(varprop)
barplot(varprop, xlab = "Principal component", ylab = "Share of variance captured [%]")
## pick 2 harmonics/principal components

## plot harmonics
plot(sppca, harm = 1, pointplot = TRUE) 
plot(sppca, harm = 2, pointplot = TRUE) 

## plot scores vs summary statistics
scoresSP <- data.frame(sppca$scores)
names(scoresSP) <- paste0("speed_pc", 1:4)
d <- cbind(runsSummary, scoresSP)

library("ggplot2")
## pc1 ~ session duration (moving)
ggplot(d) + geom_point(aes(x = as.numeric(durationMoving), y = speed_pc1))
## pc2 ~ avg speed (moving)
ggplot(d) + geom_point(aes(x = avgSpeedMoving, y = speed_pc2)) 
```