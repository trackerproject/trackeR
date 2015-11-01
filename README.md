# trackeR

### Description

The purpose of this package is to provide infrastructure for handling
cycling and running data from GPS-enabled tracking devices.

The formats that are currently supported for the training activity
files are .tcx (Training Center XML) and .db3. After extraction and
appropriate manipulation of the extracted training attributes, the
training data are placed into session-aware data objects of class
trackeRdata. The package provides methods for visualising, summarising
and analysing the information contained within trackeRdata objects.


### Current capabilities

Read:
- Read data from .tcx or .db3 files.
- Read all supported files in a specfied directory.

Data processing:
- Automatically identify training sessions from timestamps.
- Imputation of data to characterize times when the device is paused or remains stationary.
- Correction of GPS-measured distances using elevation data.
- Basic data cleaning capabilities e.g., no negative speeds or distances.
- Specify and conveniently change units of measurement.
- Organise training data into unit- and session-aware data objects of class trackeRdata.

Analysis:
- Session summaries: distance, duration, time moving, average speed/pace/heart rate/cadence/power (overall and moving), work to rest ratio.
- Time spent training in user-supplied zones, e.g., heart rate zones or speed zones.
- Training distribution profiles: time spent training above thresholds of training attributes.
- Training concentration profiles: negative derivatives of training distribution profiles.

Visualisation:
- Plot session progression in, e.g., pace, heart rate, etc.
- Plot route covered during session on maps from various providers.
- Plot session summary statistics.
- Plot time spent training in zones.
- Plot training distribution/concentration profiles.


### Installation

Install the development version from github:

```
# install.packages("devtools")
devtools::install_github("hfrick/trackeR")
```

### Example

Download example data
```{r}
con <- url("http://www.ucl.ac.uk/~ucakhfr/data/running.rda")
## print the value to see what objects were created.
print(load(con))
close(con) ## url() always opens the connection
```

Summarize sessions
```
library("trackeR")
runsSumary <- summary(runs)
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
                fdnames = c("Speed", "session", "d Time"))
		
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