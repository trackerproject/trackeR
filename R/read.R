#' Read a training file in tcx or db3 format.
#'
#' @param file The path to the file.
#' @param timezone The timezone of the observations as passed on to \code{\link[base]{as.POSIXct}}.
#' @param speedunit Character string indicating the measurement unit of the
#'     speeds in the container file to be converted into meters per second. See Details.
#' @param distanceunit Character string indicating the measurement unit of the
#'     distance in the container file to be converted into meters. See Details.
#' @param parallel Logical. Should computation be carried out in parallel? (Not supported on Windows.)
#' @param cores Number of cores for parallel computing.
#' @param ... Currently not used.
#' @details Available options for \code{speedunit} currently are \code{km_per_h}, \code{m_per_s},
#'     \code{mi_per_h}, \code{ft_per_min} and \code{ft_per_s}. The default is \code{m_per_s} for TCX files
#'     and \code{km_per_h} for db3 files.
#'     Available options for \code{distanceunit} currently are \code{km}, \code{m}, \code{mi} and
#'     \code{ft}. The default is \code{m} for TCX files and \code{km} for db3 files.
#' @export
#' @name readX
#' @examples
#' \dontrun{
#' ## read raw data
#' filepath <- system.file("extdata", "2013-06-08-090442.TCX", package = "trackeR")
#' run <- readTCX(file = filepath, timezone = "GMT")
#'
#' ## turn into trackeRdata object
#' run <- trackeRdata(run, units = data.frame(variable = c("latitude", "longitude", 
#'     "altitude", "distance", "heart.rate", "speed", "cadence", "power"),
#'     unit = c("degree", "degree", "m", "m", "bpm", "m_per_s", "steps_per_min", "W"),
#'     stringsAsFactors = FALSE))
#'
#' ## alternatively
#' run <- readContainer(filepath, type = "tcx", timezone = "GMT")
#' }
## Experimental function for reading TCX files
readTCX <- function(file, timezone = "", speedunit = "m_per_s", distanceunit = "m",
                    parallel = FALSE, cores = getOption("mc.cores", 2L),...){

    ## relevant resource: http://gastonsanchez.com/work/webdata/getting_web_data_r4_parsing_xml_html.pdf

    ## read XML file
    doc <- XML::xmlParse(file)
    nodes <- XML::getNodeSet(doc, "//ns:Trackpoint", "ns")

    ## parallelisation
    papply <- if (parallel) function(...) parallel::mclapply(..., mc.cores = cores) else lapply

    mydf <- do.call("rbind", papply(nodes, function(node) {
        ## Avoid memory leaks
        nodeDoc <- XML::xmlDoc(node)
        extnode <- XML::getNodeSet(nodeDoc, "//s:Extensions", "s")
        if (length(extnode)) {
            extnode <- extnode[[1]]
            ns <- XML::xmlNamespaceDefinitions(extnode, recursive = TRUE, simplify = TRUE)
            extnodeDoc <- XML::xmlDoc(extnode)
            ## What if speed is not in an extensions node?
            speed <- XML::xpathApply(extnodeDoc, "//o:Speed", namespaces = c(o = ns[1]), XML::xmlValue)
            ## Check if there is cadence in nodeDoc or in an Extensions tag with distinct namespace
            cadence2 <- XML::xpathApply(extnodeDoc, "//o:RunCadence", namespaces = c(o = ns[1]), XML::xmlValue)
            watts <- XML::xpathApply(extnodeDoc, "//o:Watts", namespaces = c(o = ns[1]), XML::xmlValue)
        }
        else {
            speed <- cadence2 <- watts <- list()
        }
        ## Avoid memory leaks
        cadence1 <- XML::xpathApply(nodeDoc, "//o:Cadence", namespaces = "o", XML::xmlValue)
        Len1 <- length(cadence1)
        Len2 <- length(cadence2)
        if ((!Len1 & !Len2) | (Len1 & !Len2)) cadence <- cadence1
        if ((!Len1 & Len2) | (Len1 & Len2)) cadence <- cadence2
        ## Cadence for cycling and running
        longitude <- XML::xpathApply(nodeDoc, "//o:LongitudeDegrees", namespaces = "o", XML::xmlValue)
        latitude <- XML::xpathApply(nodeDoc, "//o:LatitudeDegrees", namespaces = "o", XML::xmlValue)
        time <- XML::xpathApply(nodeDoc, "//o:Time", namespaces = "o", XML::xmlValue)
        altitude <- XML::xpathApply(nodeDoc, "//o:AltitudeMeters", namespaces = "o", XML::xmlValue)
        distance <- XML::xpathApply(nodeDoc, "//o:DistanceMeters", namespaces = "o", XML::xmlValue)
        ## What if hr is defined differently?
        hr <- XML::xpathApply(nodeDoc, "//o:HeartRateBpm", namespaces = "o", XML::xmlValue)
        nullout <- function(z) if (length(z)) z[[1]] else NA
        c(time = time[[1]],
          longitude = nullout(longitude),
          latitude = nullout(latitude),
          altitude = nullout(altitude),
          distance = nullout(distance),
          hr = nullout(hr),
          speed = nullout(speed),
          cadence = nullout(cadence),
                  watts = nullout(watts))
    })) ## , mc.cores = mc.cores

    fac2num <- function(z) {
        as.numeric(levels(z))[z]
    }

    ## Test for useable data in container file
    if (!nrow(mydf)) {
        stop("no useable data in input")
    }

    mydf <- within(as.data.frame(mydf), {
        longitude = fac2num(longitude)
        latitude = fac2num(latitude)
        altitude = fac2num(altitude)
        distance = fac2num(distance)
        hr = fac2num(hr)
        speed = fac2num(speed)
        cadence = fac2num(cadence)
        watts = fac2num(watts)
    })

    ## perpare names
    allnames <- generateVariableNames()
    namesOfInterest <- allnames$tcxNames
    namesToBeUsed <- allnames$humanNames

    ## ## handle alternative names for heart rate and speed
    ## if (!("value.HeartRateBpm.Value" %in% names(mydf)) & ("value.Value" %in% names(mydf))) {
    ##     mydf[["value.HeartRateBpm.Value"]] <- mydf[["value.Value"]]
    ## }
    ## if (!("value.Extensions.TPX.Speed" %in% names(mydf)) & ("value.Speed" %in% names(mydf))) {
    ##     mydf[["value.Extensions.TPX.Speed"]] <- mydf[["value.Speed"]]
    ## }

    ## extract the interesting variables
    inds <- match(namesOfInterest, names(mydf), nomatch = 0)
    newdat <- mydf[inds]
    names(newdat) <- namesToBeUsed[inds!=0]

    ## START hack: this is a hack for instances where only the time was
    ## recorded because if the node had only Time recordings then the
    ## record goes to the Time variable of mydf instead of the
    ## value.Time
    if ("time" %in% names(newdat)) {
        newdat$time <- as.character(newdat$time)
        newdat$time[is.na(newdat$time)] <- as.character(mydf$Time[is.na(newdat$time)])
    }
    ## END hack

    ## coerce time into POSIXct
    newdat$time <- as.POSIXct(newdat$time, format = "%Y-%m-%dT%H:%M:%OSZ",
                              tz = timezone)
    ## coerce the numeric variables into the correct class
    numVars <- which(names(newdat) != "time")
    for (i in numVars){
        newdat[,i] <- as.numeric(as.character(newdat[, i]))
    }

    ## add missing variables as NA
    missingVars <- namesToBeUsed[match(namesToBeUsed, names(newdat), nomatch = 0) == 0]
    if (nrow(newdat) > 0) {
        for (nn in missingVars) {
            newdat[[nn]] <- NA
        }
    }

    ## convert speed to m/s from speedunit
    speedConversion <- match.fun(paste(speedunit, "m_per_s", sep = "2"))
    newdat$speed <- speedConversion(newdat$speed)

    ## convert distance to m from distanceunit
    distanceConversion <- match.fun(paste("m", distanceunit, sep = "2"))
    newdat$distance <- distanceConversion(newdat$distance)

    ## use variable order for trackeRdata
    if (any(names(newdat) != allnames$humanNames))
        newdat <- newdat[, allnames$humanNames]

    return(newdat)
}



#' @param table Character string indicating the name of the table with the GPS data in the db3 container file.
#' @inheritParams readX
#' @export
#' @rdname readX
readDB3 <- function(file, timezone = "", table = "gps_data", 
                    speedunit = "km_per_h", distanceunit = "km"){

    db <- RSQLite::dbConnect(RSQLite::SQLite(), file)
    mydf <- RSQLite::dbReadTable(conn = db, name = table)
    RSQLite::dbDisconnect(db)

    ## Test for useable data in container file
    if (!nrow(mydf)) {
        stop("no useable data in input")
    }

    ## prepare names
    allnames <- generateVariableNames()
    namesOfInterest <- allnames$db3Names
    namesToBeUsed <- allnames$humanNames

    ## extract the interesting variables
    inds <- match(namesOfInterest, names(mydf), nomatch = 0)
    newdat <- mydf[inds]
    names(newdat) <- namesToBeUsed[inds!=0]

    ## coerce time into POSIXct
    newdat$time <- as.POSIXct(newdat$time*24*60*60, origin = "1899-12-30", tz = timezone)

    ## add missing variables as NA
    missingVars <- namesToBeUsed[match(namesToBeUsed, names(newdat), nomatch = 0) == 0]
    if (nrow(newdat) > 0) {
        for (nn in missingVars) {
            newdat[[nn]] <- NA
        }
    }

    ## convert speed to m/s from speedunit
    speedConversion <- match.fun(paste(speedunit, "m_per_s", sep = "2"))
    newdat$speed <- speedConversion(newdat$speed)

    ## convert distance to m from distanceunit
    distanceConversion <- match.fun(paste("m", distanceunit, sep = "2"))
    newdat$distance <- distanceConversion(newdat$distance)

    ## use variable order for trackeRdata
    if (any(names(newdat) != allnames$humanNames))
        newdat <- newdat[, allnames$humanNames]

    return(newdat)
}


#' Read a GPS container file.
#'
#' @param file The path to the file.
#' @param type The type of the GPS container file. Supported so far are \code{tcx} and \code{db3}.
#' @param table The name of the table in the database if \code{type} is set to \code{db3},
#'     ignored otherwise.
#' @param fromDistances Logical. Should the speeds be calculated from the distance recordings
#'     instead of taken from the speed recordings directly. Defaults to \code{TRUE} for \code{tcx}
#'     files and to \code{FALSE} for \code{db3} files.
#' @param speedunit Character string indicating the measurement unit of the speeds in the container
#'     file to be converted into meters per second. Default is \code{m_per_s} when \code{type} is
#'     \code{tcx} and \code{km_per_h} when \code{type} is \code{db3}. See Details.
#' @param distanceunit Character string indicating the measurement unit of the distance in the container
#'     file to be converted into meters. Default is \code{m} when \code{type} is
#'     \code{tcx} and \code{km} when \code{type} is \code{db3}. See Details.
#' @param cycling Logical. Do the data stem from cycling instead of running? If so, the unit of
#'     measurement for cadence is set to \code{rev_per_min} instead of \code{steps_per_min}.
#' @inheritParams readX
#' @inheritParams restingPeriods
#' @inheritParams imputeSpeeds
#' @inheritParams trackeRdata
#' @details  Available options for \code{speedunit} currently are \code{km_per_h}, \code{m_per_s},
#'     \code{mi_per_h}, \code{ft_per_min} and \code{ft_per_s}.
#'     Available options for \code{distanceunit} currently are \code{km}, \code{m}, \code{mi} and
#'     \code{ft}.
#' @return An object of class \code{\link{trackeRdata}}.
#' @seealso \code{\link{trackeRdata}}, \code{\link{readTCX}}, \code{\link{readDB3}}
#'
#' @export
#' @examples
#' \dontrun{
#' filepath <- system.file("extdata", "2013-06-08-090442.TCX", package = "trackeR")
#' run <- readContainer(filepath, type = "tcx", timezone = "GMT")
#' }
readContainer <- function(file, type = c("tcx", "db3"),
                          table = "gps_data", timezone = "", sessionThreshold = 2,
                          correctDistances = FALSE,
                          country = NULL, mask = TRUE,
                          fromDistances = NULL,
                          speedunit = NULL, distanceunit = NULL,
                          cycling = FALSE,
                          lgap = 30, lskip = 5, m = 11,
                          parallel = FALSE, cores = getOption("mc.cores", 2L)){
    ## prepare args
    type <- match.arg(tolower(type), choices = c("tcx", "db3"))
    if (is.null(fromDistances)){
        fromDistances <- if (type == "db3") FALSE else TRUE
    }
    if (is.null(speedunit)){
        speedunit <- switch(type, "tcx" = "m_per_s", "db3" = "km_per_h")
    }
    if (is.null(distanceunit)) {
        distanceunit <- switch(type, "tcx" = "m", "db3" = "km")
    }
    
    ## read gps data
    dat <- switch(type,
                  "tcx" = readTCX(file = file, timezone = timezone, speedunit = speedunit,
                      distanceunit = distanceunit, parallel = parallel, cores = cores),
                  "db3" = readDB3(file = file, table = table, timezone = timezone,
                      speedunit = speedunit, distanceunit = distanceunit)
                  )
    ## units of measurement 
    units <- generateBaseUnits(cycling) ## readX returns default units
    #units <- units[-which(units$variable == "duration"), ]

    ## make trackeRdata object (with all necessary data handling)
    trackerdat <- trackeRdata(dat, units = units,
                              correctDistances = correctDistances, country = country, mask = mask,
                              sessionThreshold = sessionThreshold,
                              fromDistances = fromDistances, lgap = lgap, lskip = lskip, m = m)

    return(trackerdat)
}


##' Generate variables names for internal use in readX functions. the
##' variables vecotrs need to correspond one by on interms of variable
##' type
generateVariableNames <- function() {
    humanNames <- c("time",
                    "latitude",
                    "longitude",
                    "altitude",
                    "distance",
                    "heart.rate",
                    "speed",
                    "cadence",
                    "power")

    ## resources for tcx:
    ## https://en.wikipedia.org/wiki/Training_Center_XML
    ## http://www8.garmin.com/xmlschemas/index.jsp#/web/docs/xmlschemas
    ## http://www.garmindeveloper.com/schemas/tcx/v2/
    tcxNames <- c("time",
                  "latitude",
                  "longitude",
                  "altitude",
                  "distance",
                  "hr",
                  "speed",
                  "cadence",
                  "watts")

    ## Resource for db3: none... mostly reverse engineering
    db3Names <-     c("dttm",
                      "lat",
                      "lon",
                      "altitude",
                      "dist",
                      "hr",
                      "velocity",
                      "cadence",
                      "watts")

    list(humanNames = humanNames,
         tcxNames = tcxNames,
         db3Names = db3Names)
}



## Reads supported container files from a supplied directory
## CYCLING applied to all files!
## directory should end with "/"?

#' Read all supported container files from a supplied directory.
#'
#' @param directory The path to the directory.
#' @param aggregate Logical. Aggregate data from different files to the same session if observations are less then \code{sessionThreshold} hours apart? Alternatively, data from different files is stored in different sessions.
#' @param table The name of the table in the database for db3 files.
#' @param fromDistances Logical. Should the speeds be calculated from the distance recordings
#'     instead of taken from the speed recordings directly. Defaults to \code{TRUE} for tcx
#'     files and to \code{FALSE} for db3 files.
#' @param speedunit Character string indicating the measurement unit of the speeds in the container
#'     file to be converted into meters per second. Default is \code{m_per_s} for tcx files and \code{km_per_h} for db3 files. See Details.
#' @param distanceunit Character string indicating the measurement unit of the distance in the container
#'     file to be converted into meters. Default is \code{m} for tcx files and \code{km} for db3 files. See Details.
#' @param cycling Logical. Do the data stem from cycling instead of running? If so, the default unit of
#'     measurement for cadence is set to \code{rev_per_min} instead of \code{steps_per_min} and power is
#'     imputed with \code{0}, else with \code{NA}.
#' @param verbose Logical. Should progress reports be printed?
#' @inheritParams readX
#' @inheritParams restingPeriods
#' @inheritParams imputeSpeeds
#' @inheritParams trackeRdata
#' @details Available options for \code{speedunit} currently are \code{km_per_h}, \code{m_per_s},
#'     \code{mi_per_h}, \code{ft_per_min} and \code{ft_per_s}.
#'     Available options for \code{distanceunit} currently are \code{km}, \code{m}, \code{mi} and
#'     \code{ft}.
#' @return An object of class \code{\link{trackeRdata}}.
#' @seealso \code{\link{trackeRdata}}, \code{\link{readTCX}}, \code{\link{readDB3}}
#'
#' @export
readDirectory <- function(directory,
                          aggregate = TRUE, ## aggregate data from all files or keep data from different files in different sessions?
                          table = "gps_data",
                          timezone = "",
                          sessionThreshold = 2,
                          correctDistances = FALSE,
                          country = NULL,
                          mask = TRUE,
                          fromDistances = NULL,
                          speedunit = list(tcx = "m_per_s", db3 = "km_per_h"),
                          distanceunit = list(tcx = "m", db3 = "km"),
                          cycling = FALSE,
                          lgap = 30, lskip = 5, m = 11,
                          parallel = FALSE, cores = getOption("mc.cores", 2L),
                          verbose = TRUE) {

    tcxfiles <- list.files(directory, pattern = "tcx", ignore.case = TRUE, full.names = TRUE,
                           no.. = TRUE)
    db3files <- list.files(directory, pattern = "db3", ignore.case = TRUE, full.names = TRUE,
                           no.. = TRUE)
    ltcx <- length(tcxfiles)
    ldb3 <- length(db3files)
    if ((ltcx == 0) & (ldb3 == 0)) {
        stop("The supplied directory contains no files with the supported formats.")
    }
    
    ## Read tcx files
    if (ltcx) {
        tcxData <- list()
        if (aggregate){
            for (j in seq.int(ltcx)) {
                if (verbose) cat("Reading file", tcxfiles[j], paste0("(file ", j, " out of ", ltcx, ")"), "...\n")
                tcxData[[j]] <- try(readTCX(tcxfiles[j],
                                            timezone = timezone,
                                            speedunit = speedunit$tcx,
                                            distanceunit = distanceunit$tcx,
                                            parallel = parallel,
                                            cores = cores))
            }
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
                                   m = m)
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
                                   m = m)
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
                                                  cores = cores))
            }
            if (verbose) cat("Cleaning up...")
            db3Data <- do.call("c", db3Data[!sapply(db3Data, inherits, what = "try-error")])
            if (verbose) cat("Done\n")  
        }
    } else {
        db3Data <- NULL
    }

    ## combine and return
    allData <- list(tcxData, db3Data)
    allData <- allData[!sapply(allData, is.null)]
    ret <- do.call("c", allData)
    return(ret)
}
