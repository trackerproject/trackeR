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
                try(do.call(what = paste0("read", toupper(currentType)),
                            args = list(file = allFiles[j],
                                        timezone = timezone,
                                        speedunit = speedunit[[currentType]],
                                        distanceunit = distanceunit[[currentType]])))
            }

            foreach_object <- eval(as.call(c(list(quote(foreach::foreach), j = seq.int(lall)))))
            if (parallel) {
                setup_parallel()
                allData <- foreach::`%dopar%`(foreach_object, read_fun(j))
            }
            else {
                allData <- foreach::`%do%`(foreach_object, read_fun(j))
            }


        })
        withProgress(expr = in_expression, message = 'Loading data', value = 0, quoted = TRUE)
        if (verbose) {
            cat("Cleaning up...")
        }
        sports <- sapply(allData, attr, which = "sport")
        sport_to_use <- na.omit(sports)[1]
        if (length(unique(sports)) > 1) {
            warning(directory, "has files from multiple sports and aggregate = TRUE. Assumming that all files are ", sport_to_use)
        }

        allData <- do.call("rbind", allData[!sapply(allData, inherits, what = "try-error")])
        fromDistances <- if (is.null(fromDistances)) TRUE else fromDistances
        allData <- trackeRdata(allData,
                               sessionThreshold = sessionThreshold,
                               correctDistances = correctDistances,
                               country = country,
                               mask = mask,
                               fromDistances = fromDistances,
                               sport = sport,
                               lgap = lgap,
                               lskip = lskip,
                               m = m,
                               silent = silent)
        if (verbose) {
            cat("Done\n")
        }

        ## clean and return
        allData <- allData[!sapply(allData, is.null)]
        allData
    })


    out <- reactive(read_expression, quoted = TRUE)
    return(out())

}


