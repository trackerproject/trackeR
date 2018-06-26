#' Smoother for \code{\link{trackeRdata}} objects.
#'
#' @param object An object of class \code{\link{trackeRdata}}.
#' @param session The sessions to be smoothed. Default is all sessions.
#' @param control A list of parameters for controlling the smoothing
#' process. This is passed to \code{\link{smoother_control.trackeRdata}}.
#' @param ... Arguments to be used to form the default \code{control}
#' argument if it is not supplied directly.
#'
#' @return An object of class \code{\link{trackeRdata}}.
#'
#' @seealso \code{\link{smoother_control.trackeRdata}}
#'
#' @examples
#' data('run', package = 'trackeR')
#' ## unsmoothed speeds
#' plot(run, smooth = FALSE)
#' ## default smoothing
#' plot(run, smooth = TRUE)
#' ## smoothed with some non-default options
#' runS <- smoother(run, fun = 'median', width = 20, what = 'speed')
#' plot(runS, smooth = FALSE)
#' @export
smoother.trackeRdata <- function(object, session = NULL, control = list(...), ...) {

    operations <- attr(object, "operations")

    if (!is.null(operations$smooth)) {
        warning("'object' is already the result of smoother.")
        return(object)
    }

    ## select sessions
    if (is.null(session)) {
        session <- seq_len(length(object))
    }
    object <- object[session]

    ## evaluate control argument
    control$nsessions <- length(session)
    control <- do.call("smoother_control.trackeRdata", control)

    ## Check that all what are available
    what <- match(unlist(control$what), names(object[[1]]))

    if (any(is.na(what))) {
        stop("At least one of 'what' is not available.")
    }

    smooth_fun <- function(j) {
        zoo::rollapply(object[[j]], width = control$width,  match.fun(control$fun))
    }

    foreach_object <- eval(as.call(c(list(quote(foreach::foreach),
                                              j = seq.int(nsessions(object))))))

    if (control$parallel) {
        setup_parallel()
        objectNew <- foreach::`%dopar%`(foreach_object, smooth_fun(j))
    }
    else {
        objectNew <- foreach::`%do%`(foreach_object, smooth_fun(j))
    }

    ## replace variables not in control$what with the corresponding original data
    for (k in seq_len(length(object))) {
        inds <- index(objectNew[[k]])
        objectNew[[k]][, -what] <- object[[k]][inds, -what]
    }

    class(objectNew) <- "trackeRdata"

    ## Enrich attr(objectNew, 'operations') with the control of the operation that has just
    ## been performed
    operations$smooth <- control
    attr(objectNew, "operations") <- operations
    attr(objectNew, "units") <- getUnits(object)
    attr(objectNew, "sport") <- get_sport(object)
    attr(objectNew, "file") <- attr(object, "file")
    return(objectNew)

}

#' Auxiliary function for \code{\link{smoother.trackeRdata}}. Typically used to construct
#' a control argument for \code{\link{smoother.trackeRdata}}.
#'
#' @param fun The name of the function to be matched and used to
#'     aggregate/smooth the data.
#' @param width The width of the window in which the raw observations
#'     get aggregated via function \code{fun}.
#' @param parallel Logical. Should computation be carried out in
#'     parallel? If \code{TRUE} computation is performed in parallel
#'     using the backend provided to \code{\link{foreach}}. Default is
#'     \code{FALSE}.
#' @param what Vector of the names of the variables which should be
#'     smoothed.
#' @param nsessions Vector containing the number of session. Default
#'     corresponds to all sessions belonging to the same group. Used
#'     only internally.
#' @param ... Currently not used.
#'
#' @seealso \code{\link{smoother.trackeRdata}}
#' @export
smoother_control.trackeRdata <- function(fun = "mean", width = 10, parallel = FALSE,
    what = c("speed", "heart_rate"), nsessions = NA, ...) {
    # Basic checks for the arguments
    if (!is.character(fun)) {
        stop("'fun' should be a character string")
    } else {
        match.fun(fun)
    }
    if (is.vector(what)) {
        what <- list(what)
    }
    list(fun = fun, width = width, parallel = parallel, what = what, nsessions = nsessions)
}

