## Code adapted from plyr 1.8.4.9000

setup_parallel <- function() {
  if (!requireNamespace("foreach", quietly = TRUE)) {
    # EXCLUDE COVERAGE START
    stop("foreach package required for parallel operation",
      call. = FALSE)
    # EXCLUDE COVERAGE END
  }
  if (foreach::getDoParWorkers() == 1) {
    # EXCLUDE COVERAGE START
    warning("No parallel backend registered", call. = TRUE)
    # EXCLUDE COVERAGE END
  }
}
