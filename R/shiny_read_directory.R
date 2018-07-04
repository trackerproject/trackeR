read_directory_shiny <- function(directory,
                                 timezone = "",
                                 session_threshold = 2,
                                 correct_distances = FALSE,
                                 country = NULL,
                                 mask = TRUE,
                                 from_distances = NULL,
                                 speedunit = list(tcx = "m_per_s", gpx = "km_per_h", db3 = "km_per_h", json = "km_per_h"),
                                 distanceunit = list(tcx = "m", gpx = "km", db3 = "km", json = "km"),
                                 sport = NULL,
                                 lgap = 30, lskip = 5, m = 11,
                                 silent = TRUE,
                                 parallel = FALSE,
                                 verbose = FALSE) {


    read_expression <- quote({
        tcxFiles <- list.files(directory, pattern = "tcx", ignore.case = TRUE, full.names = TRUE,
                               no.. = TRUE)
        gpxFiles <- list.files(directory, pattern = "gpx", ignore.case = TRUE, full.names = TRUE,
                               no.. = TRUE)
        db3Files <- list.files(directory, pattern = "db3", ignore.case = TRUE, full.names = TRUE,
                               no.. = TRUE)
        jsonFiles <- list.files(directory, pattern = "json", ignore.case = TRUE, full.names = TRUE,
                                no.. = TRUE)
        ltcx <- length(tcxFiles)
        lgpx <- length(gpxFiles)
        ldb3 <- length(db3Files)
        ljson <- length(jsonFiles)
        if ((ltcx == 0) & (ldb3 == 0) & (ljson == 0) & (lgpx == 0)) {
            stop("The supplied directory contains no files with the supported formats.")
        }
        lall <- ltcx + lgpx + ldb3 + ljson
        allFiles <- c(tcxFiles, gpxFiles, db3Files, jsonFiles)
        fileType <- c(rep("tcx", ltcx), rep("gpx", lgpx), rep("db3", ldb3), rep("json", ljson))

        allData <- list()

        in_expression <- quote({

            read_fun <- function(j) {
                currentType <- fileType[j]
                if (verbose) {
                    cat("Reading file", allFiles[j], paste0("(file ", j, " out of ", lall, ")"), "...\n")
                }
                try(read_container(file = allFiles[j],
                                   type = currentType,
                                   table = table,
                                   timezone = timezone,
                                   session_threshold = session_threshold,
                                   correct_distances = correct_distances,
                                   country = country,
                                   mask = mask,
                                   from_distances = from_distances,
                                   speedunit = speedunit[[currentType]],
                                   distanceunit = distanceunit[[currentType]],
                                   sport = sport,
                                   lgap = lgap,
                                   lskip = lskip,
                                   m = m,
                                   silent = silent))
            }

            foreach_object <- eval(as.call(c(list(quote(foreach::foreach), j = seq.int(lall)))))
            if (parallel) {
                setup_parallel()
                allData <- foreach::`%dopar%`(foreach_object, read_fun(j))
            }
            else {
                allData <- foreach::`%do%`(foreach_object, read_fun(j))
            }
            allData <- do.call("c", allData[!sapply(allData, inherits, what = "try-error")])
        })
        withProgress(expr = in_expression, message = 'Loading data', value = 0, quoted = TRUE)
        if (verbose) {
            cat("Cleaning up...")
        }

        if (verbose) {
            cat("Done\n")
        }

        ## clean and return
        allData <- allData[!sapply(allData, is.null)]
        if (aggregate) {
            attr(allData, "file") <- rep(NA, length(allData))
        }
        allData
    })

    out <- reactive(read_expression, quoted = TRUE)
    return(out())
}


