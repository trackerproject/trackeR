#' Smoother for \code{\link{trackeRdata}} objects.
#'
#' @param object An object of class \code{\link{trackeRdata}}.
#' @param session The sessions to be smoothed. Default is all sessions.
#' @param control A list of parameters for controlling the smoothing
#' process. This is passed to \code{\link{smootherControl.trackeRdata}}.
#' @param ... Arguments to be used to form the default \code{control}
#' argument if it is not supplied directly.
#' @return An object of class \code{\link{trackeRdata}}.
#' @seealso \code{\link{smootherControl.trackeRdata}}
#' @examples
#' data(run, package = "trackeR")
#' ## unsmoothed speeds
#' plot(run, smooth = FALSE)
#' ## default smoothing
#' plot(run, smooth = TRUE, cores = 2)
#' ## smoothed with some non-default options
#' runS <- smoother(run, fun = "median", width = 20, what = "speed", cores = 2)
#' plot(runS, smooth = FALSE)
#' @export
smoother.trackeRdata <- function(object, session = NULL, control = list(...), ...){

    operations <- attr(object, "operations")

    if (!is.null(operations$smooth)) {
        stop("'object' is already the result of smoother.")
    }

    ## select sessions
    if (is.null(session)) session <- seq_len(length(object))
    object <- object[session]

    ## evaluate control argument
    control$nsessions <- length(session)
    control <- do.call("smootherControl.trackeRdata", control)

    ## Check that all what are available
    what <- match(unlist(control$what), names(object[[1]]))

    if (any(is.na(what))) {
        stop("At least one of 'what' is not available.")
    }

    ## apply rolling window
    if (control$parallel) {
        if (.Platform$OS.type != "windows"){
            objectNew <- parallel::mclapply(object, zoo::rollapply,
                                            width = control$width, match.fun(control$fun),
                                            mc.cores = control$cores)
        } else {
            cl <- parallel::makePSOCKcluster(rep("localhost", control$cores))
            objectNew <- parallel::parLapply(cl, object, zoo::rollapply,
                                             width = control$width,
                                             match.fun(control$fun))
            parallel::stopCluster(cl)
        }
    } else {
        objectNew <- lapply(object, zoo::rollapply,
                            width = control$width, match.fun(control$fun))
    }
        
    ## replace variables not in control$what with the corresponding original data
    for (k in seq_len(length(object))) {
        inds <- index(objectNew[[k]])
        objectNew[[k]][, -what] <- object[[k]][inds, -what]
    }

    class(objectNew) <- "trackeRdata"

    ## Enrich attr(objectNew, "operations") with the control of the operation that has just been performed
    operations$smooth <- control
    attr(objectNew, "operations") <- operations
    attr(objectNew, "units") <- getUnits(object)
    return(objectNew)

}

#' Auxiliary function for \code{\link{smoother.trackeRdata}}. Typically used to construct
#' a control argument for \code{\link{smoother.trackeRdata}}.
#'
#' @param fun The name of the function to be matched and used to aggregate/smooth the data.
#' @param width The width of the window in which the raw observations
#'     get aggregated via function \code{fun}.
#' @param parallel Logical. Should computation be carried out in parallel?
#' @param cores Number of cores for parallel computing. If NULL, the number of cores is set to the value of \code{options("corese")} (on Windows) or \code{options("mc.cores")} (elsewhere), or, if the relevant option is unspecified, to half the number of cores detected.
#' @param what Vector of the names of the variables which should be smoothed.
#' @param nsessions Vector containing the number of session. Default corresponds to all sessions
#'     belonging to the same group. Used only internally.
#' @param ... Currently not used.
#' @seealso \code{\link{smoother.trackeRdata}}
#' @export
smootherControl.trackeRdata <- function(fun = "mean", width = 10,
                                        parallel = FALSE, cores = NULL,
                                        what = c("speed", "heart.rate"), nsessions = NA, ...) {
    # Basic checks for the arguments
    if (!is.character(fun)) {
        stop("'fun' should be a character string")
    }
    else {
        match.fun(fun)
    }
    if (is.vector(what)) {
        what <- list(what)
    }
    if (is.null(cores)){
        dc <- parallel::detectCores()
        if (.Platform$OS.type != "windows"){
            cores <- getOption("mc.cores", max(floor(dc/2), 1L))
        } else {
            cores <- getOption("cores", max(floor(dc/2), 1L))
        }
    }
    
    list(fun = fun,
         width = width,
         parallel = parallel,
         cores = cores,
         what = what,
         nsessions = nsessions)
}

