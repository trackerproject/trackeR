#' foo: A package for computating the notorious bar statistic.
#'
#' The foo package provides three categories of important functions:
#' foo, bar and baz.
#' 
#' @section Foo functions:
#' The foo functions ...
#'
#' @docType package
#' @name trackeR
#' @import zoo
#' @importFrom ggplot2 fortify
NULL
#> NULL

## register S3 method (need a name which doesn't conflict with e.g. the smooth function from the stats package)
#' Generic function for smoothing.
#'
#' @param object The object to be smoothed.
#' @param ... Arguments to be passed to methods.
#' @export
smoother <- function(object, ...) UseMethod("smoother")

#' Generic function for thresholding data.
#'
#' @param object The object containing the data.
#' @param variable A vector containing the names of the variables to which thresholding is applied.
#' @param lower A vector containing the corresponding lower thresholds.
#' @param upper A vector containing the corresponding upper thresholds.
#' @param ... Arguments to be passed to methods.
#' @export
threshold <- function(object, variable, lower, upper, ...) UseMethod("threshold")

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
