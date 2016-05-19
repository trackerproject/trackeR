#' trackeR: Infrastructure for running and cycling data from GPS-enabled tracking devices
#'
#' trackeR provides infrastructure for handling cycling and running
#' data from GPS-enabled tracking devices. After extraction and appropriate
#' manipulation of the training or competition attributes, the data are placed into
#' session-aware data objects with an S3 class trackeRdata. The information in the
#' resultant data objects can then be visualised, summarised and analysed through
#' corresponding flexible and extensible methods.
#'
#' @docType package
#' @name trackeR
#' @import zoo
#' @importFrom ggplot2 fortify
NULL
#> NULL

## register S3 methods (need a name which doesn't conflict with e.g. the smooth function from the stats package)

#' Generic function for smoothing.
#'
#' @param object The object to be smoothed.
#' @param ... Arguments to be passed to methods.
#' @export
smoother <- function(object, ...) UseMethod("smoother")

#' Generic function for scaling.
#'
#' @param object The object to be scaled.
#' @param ... Arguments to be passed to methods.
#' @export
scaled <- function(object, ...) UseMethod("scaled")

#' Generic function for appending data to existing files.
#'
#' @param object The object to be appended.
#' @param file The file to which \code{object} is to be appended.
#' @param ... Arguments to be passed to methods.
#' @export
append <- function(object, file, ...) UseMethod("append")

#' Generic function for retrieving the units of measurement.
#'
#' @param object The object of which the units of measurement are retrieved.
#' @param ... Arguments to be passed to methods.
#' @export
getUnits <- function(object, ...) UseMethod("getUnits")

#' Generic function for changing the units of measurement.
#'
#' @param object The object of which the units of measurement are changed.
#' @param variable The variable of which the units of measurement are changed.
#' @param unit The unit of measurement to which is changed.
#' @param ... Arguments to be passed to methods.
#' @export
changeUnits <- function(object, variable, unit, ...) UseMethod("changeUnits")

#' Generic function for retrieving the operation settings.
#'
#' @param object The object of which the units of measurement are retrieved.
#' @param ... Arguments to be passed to methods.
#' @export
getOperations <- function(object, ...) UseMethod("getOperations")

#' Generic function for calculating number of sessions
#'
#' @param object An object of class trackeRdata, trackeRdataSummary, trackeRdataZones, conProfile, or distrProfile
#'
#' @export
nsessions <- function(object, variable, unit, ...) UseMethod("nsessions")


#' Generic function for visualising the sessions on a time versus date plot
#'
#' @param object An object of class trackeRdata or trackeRdataSummary
#' @param lims An optional vector of two times in HH:MM format. Default is NULL. If supplied, the times are used to define the limits of the time axis.
#' @param ... Arguments passed to \code{\link{summary.trackeRdata}}
#' @export
#'
#' @examples
#' \dontrun{
#' data("runs", package = "trackeR")
#' ## timeline plot applied on the trackeRdata object directly and with
#' ## inferred limits for the time axis
#' timeline(runs)
#'
#' ## the same timeline plot applied on the trackeRdataSummary object
## ## with the time axis spanning between "00:01" and "23:59"
#' runSummary <- summary(runs)
#' timeline(runSummary, lims = c("00:01", "23:59"))
#' }
timeline <- function(object, lims, ...) UseMethod("timeline")
