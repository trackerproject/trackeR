#' Same function as \code{trackeR::readDirectory()}. Customized for shiny interface with a loading bar in the UI using \code{shinycssloaders} package.
#'
#' @param ... same arguments as \code{trackeR::readDirectory()}.

readDirectory_shiny <- function(input, output, session, directory,
                          aggregate = TRUE, ## aggregate data from all files or keep data from different files in different sessions?
                          table = "gps_data",
                          timezone = "",
                          sessionThreshold = 2,
                          correctDistances = FALSE,
                          country = NULL,
                          mask = TRUE,
                          fromDistances = NULL,
                          speedunit = list(tcx = "m_per_s", db3 = "km_per_h", json = "km_per_h"),
                          distanceunit = list(tcx = "m", db3 = "km", json = "km"),
                          cycling = FALSE,
                          lgap = 30, lskip = 5, m = 11,
                          silent = FALSE,
                          parallel = FALSE, cores = getOption("mc.cores", 2L),
                          verbose = TRUE) {



  data <- reactive({
  tcxfiles <- list.files(directory, pattern = "tcx", ignore.case = TRUE, full.names = TRUE,
                         no.. = TRUE)
  db3files <- list.files(directory, pattern = "db3", ignore.case = TRUE, full.names = TRUE,
                         no.. = TRUE)
  jsonfiles <- list.files(directory, pattern = "json", ignore.case = TRUE, full.names = TRUE,
                          no.. = TRUE)
  ltcx <- length(tcxfiles)
  ldb3 <- length(db3files)
  ljson <- length(jsonfiles)
  if ((ltcx == 0) & (ldb3 == 0) & (ljson == 0)) {
    stop("The supplied directory contains no files with the supported formats.")
  }

  ## Read tcx files
  if (ltcx) {
    tcxData <- list()
    if (aggregate){
      withProgress(message = 'Loading data', value = 0, {
        n <- length(seq.int(ltcx))
      for (j in seq.int(ltcx)) {
        # Increment the progress bar, and update the detail text.
        incProgress(1/n, detail = paste("Reading file", j))

        if (verbose) cat("Reading file", tcxfiles[j], paste0("(file ", j, " out of ", ltcx, ")"), "...\n")
        tcxData[[j]] <- try(readTCX(tcxfiles[j],
                                    timezone = timezone,
                                    speedunit = speedunit$tcx,
                                    distanceunit = distanceunit$tcx,
                                    parallel = parallel,
                                    cores = cores))
      }})
      if (verbose) cat("Cleaning up...")
      tcxData <- do.call("rbind", tcxData[!sapply(tcxData, inherits, what = "try-error")])
      fromDistancesTCX <- if(is.null(fromDistances)) TRUE else fromDistances
      tcxData <- trackeRdata(tcxData,
                             sessionThreshold = sessionThreshold,
                             correctDistances = correctDistances,
                             country = country,
                             mask = mask,
                             fromDistances = fromDistancesTCX,
                             cycling = cycling,
                             lgap = lgap,
                             lskip = lskip,
                             m = m,
                             silent = silent)
      if (verbose) cat("Done\n")
    } else {
      for (j in seq.int(ltcx)) {

        if (verbose) cat("Reading file", tcxfiles[j], paste0("(file ", j, " out of ", ltcx, ")"), "...\n")
        tcxData[[j]] <- try(readContainer(tcxfiles[j],
                                          type = "tcx",
                                          table = table,
                                          timezone = timezone,
                                          sessionThreshold = sessionThreshold,
                                          correctDistances = correctDistances,
                                          country = country,
                                          mask = mask,
                                          fromDistances = fromDistances,
                                          speedunit = speedunit$tcx,
                                          distanceunit = distanceunit$tcx,
                                          cycling = cycling,
                                          lgap = lgap,
                                          lskip = lskip,
                                          m = m,
                                          silent = silent,
                                          parallel = parallel,
                                          cores = cores))
      }
      if (verbose) cat("Cleaning up...")
      tcxData <- do.call("c", tcxData[!sapply(tcxData, inherits, what = "try-error")])
      if (verbose) cat("Done\n")
    }
  }
  else {
    tcxData <- NULL
  }

  ## Read db3 files
  if (ldb3) {
    db3Data <- list()
    if (aggregate){
      for (j in seq.int(ldb3)) {

        if (verbose) cat("Reading file", db3files[j], paste0("(file ", j, " out of ", ldb3, ")"), "...\n")
        db3Data[[j]] <- try(readDB3(db3files[j],
                                    table = table,
                                    timezone = timezone,
                                    speedunit = speedunit$db3,
                                    distanceunit = distanceunit$db3))
      }
      if (verbose) cat("Cleaning up...")
      db3Data <- do.call("rbind", db3Data[!sapply(db3Data, inherits, what = "try-error")])
      fromDistancesDB3 <- if(is.null(fromDistances)) FALSE else fromDistances
      db3Data <- trackeRdata(db3Data,
                             sessionThreshold = sessionThreshold,
                             correctDistances = correctDistances,
                             country = country,
                             mask = mask,
                             fromDistances = fromDistancesDB3,
                             cycling = cycling,
                             lgap = lgap,
                             lskip = lskip,
                             m = m,
                             silent = silent)
      if (verbose) cat("Done\n")
    } else {
      for (j in seq.int(ldb3)) {

        if (verbose) cat("Reading file", db3files[j], paste0("(file ", j, " out of ", ldb3, ")"), "...\n")
        db3Data[[j]] <- try(readContainer(db3files[j],
                                          type = "db3",
                                          table = table,
                                          timezone = timezone,
                                          sessionThreshold = sessionThreshold,
                                          correctDistances = correctDistances,
                                          country = country,
                                          mask = mask,
                                          fromDistances = fromDistances,
                                          speedunit = speedunit$db3,
                                          distanceunit = distanceunit$db3,
                                          cycling = cycling,
                                          lgap = lgap,
                                          lskip = lskip,
                                          m = m,
                                          silent = silent,
                                          cores = cores))
      }
      if (verbose) cat("Cleaning up...")
      db3Data <- do.call("c", db3Data[!sapply(db3Data, inherits, what = "try-error")])
      if (verbose) cat("Done\n")
    }
  } else {
    db3Data <- NULL
  }

  ## Read json files
  if (ljson) {
    jsonData <- list()
    if (aggregate){
      for (j in seq.int(ljson)) {

        if (verbose) cat("Reading file", jsonfiles[j], paste0("(file ", j, " out of ", ljson, ")"), "...\n")
        jsonData[[j]] <- try(readJSON(jsonfiles[j],
                                      timezone = timezone,
                                      speedunit = speedunit$json,
                                      distanceunit = distanceunit$json))
      }
      if (verbose) cat("Cleaning up...")
      jsonData <- do.call("rbind", jsonData[!sapply(jsonData, inherits, what = "try-error")])
      fromDistancesJSON <- if(is.null(fromDistances)) FALSE else fromDistances
      jsonData <- trackeRdata(jsonData,
                              sessionThreshold = sessionThreshold,
                              correctDistances = correctDistances,
                              country = country,
                              mask = mask,
                              fromDistances = fromDistancesJSON,
                              cycling = cycling,
                              lgap = lgap,
                              lskip = lskip,
                              m = m,
                              silent = silent)
      if (verbose) cat("Done\n")
    } else {
      for (j in seq.int(ljson)) {

        if (verbose) cat("Reading file", jsonfiles[j], paste0("(file ", j, " out of ", ljson, ")"), "...\n")
        jsonData[[j]] <- try(readContainer(jsonfiles[j],
                                           type = "json",
                                           table = table,
                                           timezone = timezone,
                                           sessionThreshold = sessionThreshold,
                                           correctDistances = correctDistances,
                                           country = country,
                                           mask = mask,
                                           fromDistances = fromDistances,
                                           speedunit = speedunit$json,
                                           distanceunit = distanceunit$json,
                                           cycling = cycling,
                                           lgap = lgap,
                                           lskip = lskip,
                                           m = m,
                                           silent = silent,
                                           cores = cores))
      }
      if (verbose) cat("Cleaning up...")
      jsonData <- do.call("c", jsonData[!sapply(jsonData, inherits, what = "try-error")])
      if (verbose) cat("Done\n")
    }
  } else {
    jsonData <- NULL
  }

  ## combine and return
  allData <- list(tcxData, db3Data, jsonData)
  allData <- allData[!sapply(allData, is.null)]
  ret <- do.call("c", allData)
  ret


  })


  return(data())
}
