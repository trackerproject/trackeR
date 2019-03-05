## trackeR 1.2.1

### New functionality
* `threshold.trackeRdata` reports on the progress of the operation if `trace = TRUE` in its arguments

### Enhancements
* Added new logo

### Bug fixes
* Fixed a bug that caused `threshold` to not threshold if applied without specifying any of variable, lower, upper and sport.


## trackeR 1.2.0

### Other changes
Updated vignettes to avoid errors during CRAN checks

## trackeR 1.1.0

### New functionality
* Multi-sport support (cycling, running, swimming)
* Comprehensive support for temperature recordings from devices
* `readGPX` for reading Strava GPX files
* `readX` functions now return objects with a `sport` attribute with values (`cycling`, `running`, `swimming`)
* `trackeRdata` objects now have a print method with basic summaries
* `ridges` method for `trackeRdata`, `distrProfile` and `conProfile` objects for ridgeline plots of concentration profiles
* `nsessions`, `sport`, `session_duration` and `session_times` methods for `trackeRdata` and `trackeRdataSummary` objects
* `auto_breaks` argument when plotting zones, distribution and concentration profiles
* `sort` and `unique` methods for `trackeRdata` objects

### Enhancements
* Numerous under-the-hood performance and design improvements
* Refactored code for `readTCX`; reading Garmin TCX is now notably faster (circa 15x faster) and more robust
* Refactored code for `summary` method for `trackeRdata` objects, making it faster (circa 10x faster)
* Code improvements in `readDirectory`
* `c` improvements for `trackeRdata` objects
* `readDirectory` can be perfromed in parallel usign foreach
* sane multi-platform parallelization across methods using `foreach`. The parallel backend and its details needs to be set by the user
* Wprime has been adapted for a multisport environment
* Enhancements to the definition of the `trackeRdata` object and the associated methods; the object now carries file and sport information

### Bug fixes
* Various bug fixes in `trackeRdata`
* Fixed bug in `scaled` method that would cause an error for single sessions
* The `aggregate` method is now doing what is supposed to

### Other changes
* Maintainer changed from Hannah Frick to Ioannis Kosmidis
* Robin Hornak joined developer team as author


## trackeR 1.0.0
* Added citation for JSS paper.

## trackeR 0.0.5

* The color palette for plots of trackeRdataZones objects is now also based on black/blue.
* The vignette Tour de trackeR and the examples for `plotRoute()` now use maps from Stamen rather than OpenStreetMap.

## trackeR 0.0.4

* The sanity checks performed when creating a trackeRdata object now throw warnings. This can be switched off with the argument `silent = TRUE`.
* The color palette for plots of trackeRdataSummary and trackeRdataZones objects changed slightly.

## trackeR 0.0.3
* `plotRoute()` can now include more than one session in one plot. The
  new function `leafletRoute()` uses the leaflet package to produce an
  interactive map.
* Added a method for distribution and concentration profiles to fit a functional principal components analysis and a plot function to accompany it.
* Added a second, shorter vignette "Tour de trackeR" to illustrate basic features and new functionality.
* Added a new timeline plot for trackeRdata object to visualise the date time of the sessions.
* Added a new nsessions method to access the number of sessions in various trackeR objects.
* Updated "runs" data object by splitting former session 20 into 2 sessions as the two parts of the session took place in two different place with a break of over 1.5 hours between them.


## trackeR 0.0.2
* The scale options has been removed from the distribution profile and is now set-up as a separate operation. In this implementation first smoothing and then scaling (the right order for those operations) is possible.
* Some improvements for trackeRdata(): session containing no information beyond the timestamps are removed; conversions between distance and speed now recognise the respective units.
* Distribution profile: if all values of the variable for which the profile is to be calculated are missing, the profile (and its smoothed version) will also consist of only NA (rather than throwing an error).
* Experimental support for reading Golden Cheetah's JSON files.

## trackeR 0.0.1

* First CRAN release of new "trackeR" package which provides
  infrastructure for handling running and cycling data from
  GPS-enabled tracking devices. See vignette("trackeR", package =
  "trackeR") for details.
