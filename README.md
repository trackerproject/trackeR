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