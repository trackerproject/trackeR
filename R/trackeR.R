#' trackeR: Infrastructure for running and cycling data from GPS-enabled tracking devices
#'
#' trackeR provides infrastructure for handling cycling and running
#' data from GPS-enabled tracking devices. After extraction and appropriate
#' manipulation of the training or competition attributes, the data are placed into
#' session-aware data objects with an S3 class trackeRdata. The information in the
#' resultant data objects can then be visualised, summarised and analysed through
#' corresponding flexible and extensible methods.
#'
#' @section Note:
#'
#' Core facilities in the trackeR package, including reading functions
#' (see \code{\link{readX}}), data pre-processing strategies (see
#' \code{\link{trackeRdata}}), and calculation of concentration and
#' distribution profiles (see \code{\link{distributionProfile}} and
#' \code{\link{concentrationProfile}}) are based on un-packaged R code
#' that was developed by Ioannis Kosmidis for the requirements of the
#' analyses in Kosmidis & Passfield (2015).
#'
#' @note
#'
#' This work has been supported by the English Institute of Sport
#' \url{http://www.eis2win.co.uk} and University College London (UCL),
#' which jointly contributed to the grant that funded Hannah Frick's
#' Post Doctoral Research Fellowship at UCL between 2014 and 2016 and
#' a percentage of Ioannis Kosmidis' time. Ioannis Kosmidis has also
#' been supported by the Alan Turing Institute under the EPSRC grant
#' EP/N510129/1 (Turing award number TU/B/000082). The support of the
#' aforementioned organisations is greatly acknowledged.
#'
#' The trackeR dashboard \code{\link{trackeR_app}} has been designed
#' and developed by Robin Hornak and Ioannis Kosmidis, while Robin
#' Hornak was completing his undergraduate research project in the
#' Department of Statistical Science, University College London under
#' the supervision of Ioannis Kosmidis.
#'
#' Hannah Frick maintained trackeR from up until version 1.0.0.
#'
#'
#' @references Frick, H., Kosmidis, I. (2017). trackeR: Infrastructure for Running and Cycling Data from GPS-Enabled Tracking Devices in R. \emph{Journal of Statistical Software}, \bold{82}(7), 1--29. doi:10.18637/jss.v082.i07
#' Kosmidis, I., and Passfield, L. (2015). Linking the Performance of
#'     Endurance Runners to Training and Physiological Effects via Multi-Resolution
#'     Elastic Net. \emph{ArXiv e-print} arXiv:1506.01388.
#' @docType package
#' @name trackeR
#' @import zoo
#' @import shiny
#' @import xml2
#' @import ggplot2
#' @importFrom stats quantile
#' @importFrom plotly "%>%"
#' @importFrom graphics plot
#' @importFrom stats na.omit start density
NULL
# > NULL

## Define global variables
if (getRversion() >= "2.15.1") {
    utils::globalVariables(c("Series", # plot_selectedWorkouts, ridges.trackeRdata
                             "j")) # parallelization

}

## register S3 methods (need a name which doesn't conflict with e.g. the smooth function
## from the stats package)

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

#' Generic function for extracting the units of measurement.
#'
#' @param object The object of which the units of measurement are retrieved.
#' @param ... Arguments to be passed to methods.
#' @export
get_units <- function(object, ...) UseMethod("get_units")

#' Generic function for changing the units of measurement.
#'
#' @param object The object of which the units of measurement are changed.
#' @param variable The variable of which the units of measurement are changed.
#' @param unit The unit of measurement to which is changed.
#' @param ... Arguments to be passed to methods.
#' @export
change_units <- function(object, variable, unit, ...) UseMethod("change_units")

#' Generic function for retrieving the operation settings.
#'
#' @param object The object of which the units of measurement are retrieved.
#' @param ... Arguments to be passed to methods.
#' @export
get_operations <- function(object, ...) UseMethod("get_operations")

#' Generic function for calculating number of sessions.
#'
#' @param object The object for which to calculate the number of sessions.
#' @param ... Arguments to be passed to methods.
#' @export
nsessions <- function(object, ...) UseMethod("nsessions")

#' Generic function for calculating session times.
#'
#' @param object The object for which to calculate session start and end times.
#' @param ... Arguments to be passed to methods.
#' @export
session_times <- function(object, ...) UseMethod("session_times")

#' Generic function for extracting sports
#'
#' @param object The object from which to extract sports.
#' @param ... Arguments to be passed to methods.
#' @export
get_sport <- function(object, ...) UseMethod("sport")


#' Generic function for calculating session durations.
#'
#' @param object The object for which to calculate session durations.
#' @param ... Arguments to be passed to methods.
#'
#' @details
#' The times units will be inherited from \code{object}.
#'
#' @export
session_duration <- function(object, ...) UseMethod("session_duration")


#' Generic function for visualising the sessions on a time versus date plot.
#'
#' @param object An object of class trackeRdata or trackeRdataSummary
#' @param lims An optional vector of two times in HH:MM format. Default is NULL.
#'     If supplied, the times are used to define the limits of the time axis.
#' @param ... Arguments passed to \code{\link{summary.trackeRdata}}
#' @export
#'
#' @examples
#' \dontrun{
#' data('runs', package = 'trackeR')
#' ## timeline plot applied on the \code{trackeRdata} object directly and with
#' ## inferred limits for the time axis
#' timeline(runs)
#'
#' ## the same timeline plot applied on the \code{trackeRdataSummary} object
## ## with the time axis spanning between '00:01' and '23:59'
#' runSummary <- summary(runs)
#' timeline(runSummary, lims = c('00:01', '23:59'))
#' }
timeline <- function(object, lims, ...) UseMethod("timeline")

#' Generic function for functional principal components analysis.
#'
#' @param object The object to which a functional principal components analysis is applied.
#' @param ... Arguments to be passed to methods.
#' @export
funPCA <- function(object, ...) UseMethod("funPCA")

#' Generic function for ridgeline plots.
#'
#' @param x An object of class \code{distrProfile} or \code{conProfile}.
#' @param ... Arguments to be passed to methods.
ridges <- function(x, ...) UseMethod("ridges")

