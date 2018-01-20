trackeR
=======

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/trackeR)](http://cran.r-project.org/package=trackeR)

### Description

The purpose of this package is to provide infrastructure for handling
running and cycling data from GPS-enabled tracking devices.

The formats that are currently supported for the training activity files
are .tcx (Training Center XML) and .db3. After extraction and
appropriate manipulation of the training or competition attributes, the
data are placed into session-based and unit-aware data objects of class
trackeRdata (S3 class). The information in the resultant data objects
can then be visualised, summarised, and analysed through corresponding
flexible and extensible methods.

### Current capabilities

Read:

-   Read data from .tcx, Strava .gpx, .db3 or [Golden
    Cheetah](http://goldencheetah.org)'s .json files.
-   Read all supported files in a specified directory.

Data processing:

-   Automatically identify sessions from timestamps.
-   Imputation of data to characterise times when the device is paused
    or remains stationary.
-   Correction of GPS-measured distances using elevation data.
-   Basic data cleaning capabilities e.g., no negative speeds or
    distances.
-   Specify and conveniently change units of measurement.
-   Organise data into session-based and unit-aware data objects of
    class trackeRdata.

Analysis:

-   Session summaries: distance, duration, time moving, average
    speed/pace/heart rate/cadence/power (overall and moving), work to
    rest ratio.
-   Time spent exercising in user-supplied zones, e.g., heart rate zones
    or speed zones.
-   Work capacity above critical power (W', W prime)
-   Distribution profiles: time spent exercising above thresholds of
    training attributes.
-   Concentration profiles: negative derivatives of distribution
    profiles.
-   Functional principal components analysis of distribution and
    concentration profiles.

Visualisation:

-   Plot session progression in, e.g., pace, heart rate, etc.
-   Plot route covered during session on static and interactive maps
    from various providers.
-   Plot session summary statistics.
-   Plot date time of sessions in timeline plots.
-   Plot time spent exercising in zones.
-   Plot distribution/concentration profiles.
-   Plot principal components of distribution/concentration profiles.
-   Ridgeline (or joy) plots for distribution/concentration prifiles.

Dashboard: - `trackeR_app()` launches a dashboard that gives access to
many of **trackeR**'s capabilities and provides a workflow for loading
and updating a training database within R.

### Installation

Install the released version from CRAN:

    install.packages("trackeR")

Or the development version from github:

    # install.packages("devtools")
    devtools::install_github("hfrick/trackeR")

### Dashboard

    library("trackeR")
    trackeR_app()

### Example

Summarise sessions

    library("trackeR")
    data(runs, package = "trackeR")
    runsSummary <- summary(runs)
    plot(runsSummary, group = c("total", "moving"),
        what = c("avgSpeed", "distance", "duration", "avgHeartRate"))

![](README_files/figure-markdown_github/summary-1.png)

Generate distribution and concentration profiles

    runsT <- threshold(runs)
    dpRuns <- distributionProfile(runsT, what = "speed")
    dpRunsS <- smoother(dpRuns)
    cpRuns <- concentrationProfile(dpRunsS)
    plot(cpRuns, multiple = TRUE, smooth = FALSE)

![](README_files/figure-markdown_github/cprofile-1.png)

A ridgeline plot of the concentration profiles

    ridges(cpRuns)

![](README_files/figure-markdown_github/cprofile-ridges-1.png)

Explore concentration profiles for speed, e.g., via functional principal
components analysis (PCA)

    ## fit functional PCA
    cpPCA <- funPCA(cpRuns, what = "speed", nharm = 4)

    ## pick first 2 harmonics/principal components
    round(cpPCA$varprop, 2)

    ## [1] 0.66 0.25 0.06 0.02

    ## plot harmonics
    plot(cpPCA, harm = 1:2)

![](README_files/figure-markdown_github/funPCA-1.png)

    ## plot scores vs summary statistics
    scoresSP <- data.frame(cpPCA$scores)
    names(scoresSP) <- paste0("speed_pc", 1:4)
    d <- cbind(runsSummary, scoresSP)

    library("ggplot2")
    ## pc1 ~ session duration (moving)
    ggplot(d) + geom_point(aes(x = as.numeric(durationMoving), y = speed_pc1)) + theme_bw()

![](README_files/figure-markdown_github/scores-1.png)

    ## pc2 ~ avg speed (moving)
    ggplot(d) + geom_point(aes(x = avgSpeedMoving, y = speed_pc2)) + theme_bw()

![](README_files/figure-markdown_github/scores-2.png)
