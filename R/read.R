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

    ## Resource for Golden Cheetah JSON: reverse engineering
    jsonNames <- c("SECS",
                   "LAT",
                   "LON",
                   "ALT",
                   "KM",
                   "HR",
                   "KPH",
                   "CAD",
                   "WATTS")

    list(humanNames = humanNames,
         gpxNames = tcxNames,
         tcxNames = tcxNames,
         db3Names = db3Names,
         jsonNames = jsonNames)
}


#' Read a training file in tcx, gpx, db3 or Golden Cheetah's JSON format.
#'
#' @param file The path to the file.
#' @param timezone The timezone of the observations as passed on to
#'     \code{\link[base]{as.POSIXct}}.  Ignored for JSON files.
#' @param speedunit Character string indicating the measurement unit
#'     of the speeds in the container file to be converted into meters
#'     per second. See Details.
#' @param distanceunit Character string indicating the measurement
#'     unit of the distance in the container file to be converted into
#'     meters. See Details.
#' @param parallel Logical. Should computation be carried out in
#'     parallel? (Not supported on Windows.)
#' @param cores Number of cores for parallel computing.
#' @param ... Currently not used.
#' @details Available options for \code{speedunit} currently are
#'     \code{km_per_h}, \code{m_per_s}, \code{mi_per_h},
#'     \code{ft_per_min} and \code{ft_per_s}. The default is
#'     \code{m_per_s} for TCX files and \code{km_per_h} for db3 and
#'     Golden Cheetah's json files.  Available options for
#'     \code{distanceunit} currently are \code{km}, \code{m},
#'     \code{mi} and \code{ft}. The default is \code{m} for TCX and
#'     \code{km} for gpx, db3 and Golden Cheetah's json files.
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
    if (is.null(mydf)) {
        warning(paste("no useable data in", file))
        return(NULL)
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
    newdat$time <- gsub("[\t\n]", "", newdat$time)
    newdat$time <- convertTCXTimes2POSIXct(newdat$time, timezone = timezone)
    ## newdat$time <- as.POSIXct(newdat$time, format = "%Y-%m-%dT%H:%M:%OSZ",
    ##                           tz = timezone)


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

    ## convert speed from speedunit to m/s
    if (speedunit != "m_per_s"){
        speedConversion <- match.fun(paste(speedunit, "m_per_s", sep = "2"))
        newdat$speed <- speedConversion(newdat$speed)
    }

    ## convert distance from distanceunit to m
    if (distanceunit != "m"){
        distanceConversion <- match.fun(paste(distanceunit, "m", sep = "2"))
        newdat$distance <- distanceConversion(newdat$distance)
    }

    ## use variable order for trackeRdata
    if (any(names(newdat) != allnames$humanNames))
        newdat <- newdat[, allnames$humanNames]

    return(newdat)
}

#' @inheritParams readX
#' @export
#' @rdname readX
readGPX <- function(file, timezone = "", speedunit = "km_per_h", distanceunit = "km",
                    parallel = FALSE, cores = getOption("mc.cores", 2L),...){

    ## resources:
    ## https://strava.github.io/api/v3/uploads/
    ## https://support.strava.com/hc/en-us/articles/216918437-Exporting-your-Data-and-Bulk-Export
    ## http://www.topografix.com/gpx.asp

    ## read XML file
    doc <- XML::xmlParse(file)
    nodes <- XML::getNodeSet(doc, "//ns:trkpt", "ns")

    ## parallelisation
    papply <- if (parallel) function(...) parallel::mclapply(..., mc.cores = cores) else lapply

    mydf <- do.call("rbind", papply(nodes, function(node) {
        ## Avoid memory leaks
        nodeDoc <- XML::xmlDoc(node)
        extnode <- XML::getNodeSet(nodeDoc, "//s:extensions", "s")
        if (length(extnode)) {
            extnode <- extnode[[1]]
            extnodeDoc <- XML::xmlDoc(extnode)
            temperature <- XML::xpathApply(extnodeDoc, "//gpxtpx:atemp", fun = XML::xmlValue)
            cadence <- XML::xpathApply(extnodeDoc, "//gpxtpx:cad", fun = XML::xmlValue)
            hr <- XML::xpathApply(extnodeDoc, "//gpxtpx:hr", fun = XML::xmlValue)
            speed <- altitude <- watts <- list()
        }
        else {
            speed <- altitude <- temperature <- cadence <- hr <- watts <- list()
        }
        longitude <- XML::xmlGetAttr(node, "lon")
        latitude <- XML::xmlGetAttr(node, "lat")
        time <- XML::xpathApply(nodeDoc, "//o:time", namespaces = "o", XML::xmlValue)
        elevation <- XML::xpathApply(nodeDoc, "//o:ele", namespaces = "o", XML::xmlValue)
        nullout <- function(z) if (length(z)) z[[1]] else NA
        c(time = time[[1]],
          longitude = nullout(longitude),
          latitude = nullout(latitude),
          altitude = nullout(altitude),
          elevation = nullout(elevation),
          hr = nullout(hr),
          speed = nullout(speed),
          cadence = nullout(cadence),
          watts = nullout(watts))
    }))

    fac2num <- function(z) {
        as.numeric(levels(z))[z]
    }

    ## Test for useable data in container file
    if (is.null(mydf)) {
        warning(paste("no useable data in", file))
        return(NULL)
    }

    distance <- cumsum(c(0, sp::spDists(mydf[, c("longitude", "latitude")], longlat = TRUE, segments = TRUE)))

    mydf <- within(as.data.frame(mydf), {
        longitude = fac2num(longitude)
        latitude = fac2num(latitude)
        altitude = fac2num(altitude)
        hr = fac2num(hr)
        elevation = fac2num(elevation)
        speed = fac2num(speed)
        cadence = fac2num(cadence)
        watts = fac2num(watts)
    })

    mydf$distance <- distance

    ## perpare names
    allnames <- generateVariableNames()
    namesOfInterest <- allnames$gpxNames
    namesToBeUsed <- allnames$humanNames

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
    newdat$time <- gsub("[\t\n]", "", newdat$time)
    newdat$time <- convertTCXTimes2POSIXct(newdat$time, timezone = timezone)
    ## newdat$time <- as.POSIXct(newdat$time, format = "%Y-%m-%dT%H:%M:%OSZ",
    ##                           tz = timezone)


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

    ## convert speed from speedunit to m/s
    if (speedunit != "m_per_s"){
        speedConversion <- match.fun(paste(speedunit, "m_per_s", sep = "2"))
        newdat$speed <- speedConversion(newdat$speed)
    }

    ## convert distance from distanceunit to m
    if (distanceunit != "m"){
        distanceConversion <- match.fun(paste(distanceunit, "m", sep = "2"))
        newdat$distance <- distanceConversion(newdat$distance)
    }


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

    ## convert speed from speedunit to m/s
    if (speedunit != "m_per_s"){
        speedConversion <- match.fun(paste(speedunit, "m_per_s", sep = "2"))
        newdat$speed <- speedConversion(newdat$speed)
    }

    ## convert distance from distanceunit to m
    if (distanceunit != "m"){
        distanceConversion <- match.fun(paste(distanceunit, "m", sep = "2"))
        newdat$distance <- distanceConversion(newdat$distance)
    }

    ## use variable order for trackeRdata
    if (any(names(newdat) != allnames$humanNames))
        newdat <- newdat[, allnames$humanNames]

    return(newdat)
}


#' @inheritParams readX
#' @details Reading Golden Cheetah's JSON files is experimental.
#' @export
#' @rdname readX
readJSON <- function(file, timezone = "", speedunit = "km_per_h", distanceunit = "km"){
    ## get all data
    jslist <- jsonlite::fromJSON(file)$RIDE

    ## starting time
    stime <- strsplit(jslist$STARTTIME, " ")[[1]]
    if (timezone == "") timezone <- stime[3]
    stime <- as.POSIXct(strptime(paste(stime[1:2], collapse = "T"),
                                 format = "%Y/%m/%dT%H:%M:%S"), tz = timezone)

    ## tracking data
    if (!("SAMPLES" %in% names(jslist))) stop("No tracking data available.")
    mydf <- jslist$SAMPLES

    ## prepare names
    allnames <- generateVariableNames()
    namesOfInterest <- allnames$jsonNames
    namesToBeUsed <- allnames$humanNames

    ## extract the interesting variables
    inds <- match(namesOfInterest, names(mydf), nomatch = 0)
    newdat <- mydf[inds]
    names(newdat) <- namesToBeUsed[inds!=0]

    ## coerce time into POSIXct
    newdat$time <- stime + newdat$time

    ## add missing variables as NA
    missingVars <- namesToBeUsed[match(namesToBeUsed, names(newdat), nomatch = 0) == 0]
    if (nrow(newdat) > 0) {
        for (nn in missingVars) {
            newdat[[nn]] <- NA
        }
    }

    ## convert speed from speedunit to m/s
    if (speedunit != "m_per_s"){
        speedConversion <- match.fun(paste(speedunit, "m_per_s", sep = "2"))
        newdat$speed <- speedConversion(newdat$speed)
    }

    ## convert distance from distanceunit to m
    if (distanceunit != "m"){
        distanceConversion <- match.fun(paste(distanceunit, "m", sep = "2"))
        newdat$distance <- distanceConversion(newdat$distance)
    }

    ## use variable order for trackeRdata
    if (any(names(newdat) != allnames$humanNames))
        newdat <- newdat[, allnames$humanNames]

    return(newdat)

}


#' Read a GPS container file.
#'
#' @param file The path to the file.
#' @param type The type of the GPS container file. Supported so far are \code{tcx}, \code{db3}, and \code{json}.
#' @param table The name of the table in the database if \code{type} is set to \code{db3},
#'     ignored otherwise.
#' @param fromDistances Logical. Should the speeds be calculated from the distance recordings
#'     instead of taken from the speed recordings directly. Defaults to \code{TRUE} for \code{tcx}
#'     and Golden Cheetah's json files and to \code{FALSE} for \code{db3} files.
#' @param speedunit Character string indicating the measurement unit of the speeds in the container
#'     file to be converted into meters per second. Default is \code{m_per_s} when \code{type} is
#'     \code{tcx} and \code{km_per_h} when \code{type} is \code{db3} or \code{json}. See Details.
#' @param distanceunit Character string indicating the measurement unit of the distance in the container
#'     file to be converted into meters. Default is \code{m} when \code{type} is
#'     \code{tcx} and \code{km} when \code{type} is \code{db3} or \code{json}. See Details.
#' @param cycling Logical. Do the data stem from cycling instead of running? If so, the unit of
#'     measurement for cadence is set to \code{rev_per_min} instead of \code{steps_per_min}.
#' @inheritParams readX
#' @inheritParams restingPeriods
#' @inheritParams imputeSpeeds
#' @inheritParams trackeRdata
#' @inheritParams sanityChecks
#' @details  Available options for \code{speedunit} currently are \code{km_per_h}, \code{m_per_s},
#'     \code{mi_per_h}, \code{ft_per_min} and \code{ft_per_s}.
#'     Available options for \code{distanceunit} currently are \code{km}, \code{m}, \code{mi} and
#'     \code{ft}.
#'
#'     Reading Golden Cheetah's JSON files is experimental.
#' @return An object of class \code{\link{trackeRdata}}.
#' @seealso \code{\link{trackeRdata}}, \code{\link{readTCX}}, \code{\link{readDB3}}, \code{\link{readJSON}}
#'
#' @export
#' @examples
#' \dontrun{
#' filepath <- system.file("extdata", "2013-06-08-090442.TCX", package = "trackeR")
#' run <- readContainer(filepath, type = "tcx", timezone = "GMT")
#' }
readContainer <- function(file, type = c("tcx", "gpx", "db3", "json"),
                          table = "gps_data", timezone = "", sessionThreshold = 2,
                          correctDistances = FALSE,
                          country = NULL, mask = TRUE,
                          fromDistances = NULL,
                          speedunit = NULL, distanceunit = NULL,
                          cycling = FALSE,
                          lgap = 30, lskip = 5, m = 11,
                          silent = FALSE,
                          parallel = FALSE, cores = getOption("mc.cores", 2L)){
    ## prepare args
    type <- match.arg(tolower(type), choices = c("tcx", "gpx", "db3", "json"))
    if (is.null(fromDistances)){
        fromDistances <- if (type == "db3") FALSE else TRUE
    }
    if (is.null(speedunit)){
        speedunit <- switch(type,
                            "tcx" = "m_per_s",
                            "gpx" = "km_per_h",
                            "db3" = "km_per_h",
                            "json" = "km_per_h")
    }
    if (is.null(distanceunit)) {
        distanceunit <- switch(type,
                               "tcx" = "m",
                               "gpx" = "km",
                               "db3" = "km",
                               "json" = "km")
    }

    ## read gps data
    dat <- switch(type,
                  "tcx" = readTCX(file = file, timezone = timezone, speedunit = speedunit,
                                  distanceunit = distanceunit, parallel = parallel, cores = cores),
                  "gpx" = readGPX(file = file, timezone = timezone, speedunit = speedunit,
                                  distanceunit = distanceunit, parallel = parallel, cores = cores),
                  "db3" = readDB3(file = file, table = table, timezone = timezone,
                                  speedunit = speedunit, distanceunit = distanceunit),
                  "json" = readJSON(file = file, timezone = timezone, speedunit = speedunit,
                      distanceunit = distanceunit)
                  )
    ## units of measurement
    units <- generateBaseUnits(cycling) ## readX returns default units
    #units <- units[-which(units$variable == "duration"), ]


    ## make trackeRdata object (with all necessary data handling)
    trackerdat <- trackeRdata(dat, units = units, cycling = cycling,
                              correctDistances = correctDistances, country = country, mask = mask,
                              sessionThreshold = sessionThreshold,
                              fromDistances = fromDistances,
                              lgap = lgap, lskip = lskip, m = m,
                              silent = silent)

    return(trackerdat)
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
#'     instead of taken from the speed recordings directly. Defaults to \code{TRUE} for tcx and
#'     Golden Cheetah's json files and to \code{FALSE} for db3 files.
#' @param speedunit Character string indicating the measurement unit of the speeds in the container
#'     file to be converted into meters per second. Default is \code{m_per_s} for tcx files and \code{km_per_h} for db3 and Golden Cheetah's json files. See Details.
#' @param distanceunit Character string indicating the measurement unit of the distance in the container
#'     file to be converted into meters. Default is \code{m} for tcx files and \code{km} for db3 and Golden Cheetah's json files. See Details.
#' @param cycling Logical. Do the data stem from cycling instead of running? If so, the default unit of
#'     measurement for cadence is set to \code{rev_per_min} instead of \code{steps_per_min} and power is
#'     imputed with \code{0}, else with \code{NA}.
#' @param verbose Logical. Should progress reports be printed?
#' @param shiny Logical. Should the output of readDirectory be made \code{\link[shiny]{reactive}}? For use in the shiny interface. Default is \code{FALSE}.
#' @inheritParams readX
#' @inheritParams restingPeriods
#' @inheritParams imputeSpeeds
#' @inheritParams trackeRdata
#' @inheritParams sanityChecks
#' @details Available options for \code{speedunit} currently are \code{km_per_h}, \code{m_per_s},
#'     \code{mi_per_h}, \code{ft_per_min} and \code{ft_per_s}.
#'     Available options for \code{distanceunit} currently are \code{km}, \code{m}, \code{mi} and
#'     \code{ft}.
#'
#'     Reading Golden Cheetah's JSON files is experimental.
#' @return An object of class \code{\link{trackeRdata}}.
#' @seealso \code{\link{trackeRdata}}, \code{\link{readTCX}}, \code{\link{readDB3}}, \code{\link{readJSON}}
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
                          speedunit = list(tcx = "m_per_s", gpx = "km_per_h", db3 = "km_per_h", json = "km_per_h"),
                          distanceunit = list(tcx = "m", gpx = "km", db3 = "km", json = "km"),
                          cycling = FALSE,
                          lgap = 30, lskip = 5, m = 11,
                          silent = FALSE,
                          shiny = FALSE, ## only relevant for shiny interfaces
                          parallel = FALSE, cores = getOption("mc.cores", 2L),
                          verbose = TRUE) {

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

        if (aggregate) {
            in_expression <- quote({
                for (j in seq.int(lall)) {
                    currentType <- fileType[j]
                    if (shiny) {
                        incProgress(1/lall, detail = paste(j, "out of", lall,
                                                           paste0("(", currentType, ")")))
                    }
                    if (verbose) {
                        cat("Reading file", allFiles[j], paste0("(file ", j, " out of ", lall, ")"), "...\n")
                    }
                    allData[[j]] <- try(do.call(what = paste0("read", toupper(currentType)),
                                                args = list(file = allFiles[j],
                                                            timezone = timezone,
                                                            speedunit = speedunit[[currentType]],
                                                            distanceunit = distanceunit[[currentType]],
                                                            parallel = parallel,
                                                            cores = cores)))
                }
            })
            if (shiny) {
                withProgress(expr = in_expression, message = 'Loading data', value = 0, quoted = TRUE)
            }
            else {
                eval(in_expression)
            }
            if (verbose) {
                cat("Cleaning up...")
            }
            allData <- do.call("rbind", allData[!sapply(allData, inherits, what = "try-error")])
            fromDistances <- if (is.null(fromDistances)) TRUE else fromDistances
            allData <- trackeRdata(allData,
                                   sessionThreshold = sessionThreshold,
                                   correctDistances = correctDistances,
                                   country = country,
                                   mask = mask,
                                   fromDistances = fromDistances,
                                   cycling = cycling,
                                   lgap = lgap,
                                   lskip = lskip,
                                   m = m,
                                   silent = silent)
            if (verbose) {
                cat("Done\n")
            }
        }
        else {
            in_expression <- quote({
                for (j in seq.int(lall)) {
                    currentType <- fileType[j]
                    if (shiny) {
                        incProgress(1/lall, detail = paste(j, "out of", lall,
                                                           paste0("(", currentType, ")")))
                    }
                    if (verbose) {
                        cat("Reading file", allFiles[j], paste0("(file ", j, " out of ", lall, ")"), "...\n")
                    }
                    allData[[j]] <- try(readContainer(file = allFiles[j],
                                                      type = currentType,
                                                      table = table,
                                                      timezone = timezone,
                                                      sessionThreshold = sessionThreshold,
                                                      correctDistances = correctDistances,
                                                      country = country,
                                                      mask = mask,
                                                      fromDistances = fromDistances,
                                                      speedunit = speedunit[[currentType]],
                                                      distanceunit = distanceunit[[currentType]],
                                                      cycling = cycling,
                                                      lgap = lgap,
                                                      lskip = lskip,
                                                      m = m,
                                                      silent = silent,
                                                      parallel = parallel,
                                                      cores = cores))
                }
            })
            if (shiny) {
                withProgress(expr = in_expression, message = 'Loading data', value = 0, quoted = TRUE)
            }
            else {
                eval(in_expression)
            }
            if (verbose) {
                cat("Cleaning up...")
            }
            allData <- do.call("c", allData[!sapply(allData, inherits, what = "try-error")])
            if (verbose) {
                cat("Done\n")
            }
        }

        ## clean and return
        allData <- allData[!sapply(allData, is.null)]
        allData
    })

    if (shiny) {
        out <- reactive(read_expression, quoted = TRUE)
        return(out())
    }
    else {
        eval(read_expression)
    }

}

readDirectory_shiny <- function(input,
                                   output,
                                   session,
                                   ...) {
    readDirectory(..., shiny = TRUE)
}



## helper functions
removeColon <- function(x){
    sapply(strsplit(x, split = ":"), paste, collapse = "")
}

convertTCXTimes2POSIXct <- function(x, timezone = ""){

    ## get first non-NA element to determine the format
    formatSample <- x[which.min(is.na(x))]

    ## set basis for format
    frm <- "%Y-%m-%dT%H:%M:"

    if (nchar(formatSample) <= 19L) {
        ## just 2 characters for the seconds, nothing else
        frm <- paste0(frm, "%S")
    } else {

        rest <- substr(formatSample, start = 20, stop = nchar(formatSample))

        if (substr(rest, 1, 1) %in% c(".", ",")) {
            rest <- substr(rest, start = 2, stop = nchar(rest))

            ## determine the number of digits for the seconds
            splitted <- strsplit(rest, split = "")[[1]]
            ndigits <- which.min(splitted %in% c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")) - 1

            ## remove any digits beyond 6
            if (ndigits > 6){
                x <- paste0(substr(x, 1, 26), substr(x, 20 + ndigits + 1, nchar(formatSample)))
            }

            ## update format
            frm <- paste0(frm, "%OS")#, min(ndigits, 6))

            ## get remainder beyond seconds for timezone specification
            rest <- substr(rest, ndigits + 1, nchar(rest))

        } else {
            ## add seconds to format
            frm <- paste0(frm, "%S")
            ndigits <- 0
        }

        ## work with remainder to check for timezone specification

        if (rest != ""){
            if (substr(rest, 1, 1)  == "Z"){
                if (!(timezone %in% c("GMT", "UCT")) & timezone != "")
                    warning("Time zone will be UTC as recorded in the TCX file.")
                timezone <- "UTC"
                ##x <- substr(x, start = 1, stop = 19)
                ##frm <- "%Y-%m-%dT%H:%M:%S"
            }
            if (substr(rest, 1, 1) %in% c("-", "+")) { ## include hyphen?
                base <- 19 + ifelse(ndigits < 1, 0, min(ndigits, 6) + 1) ## +1 corresponds to "."
                x <- paste0(substr(x, start = 1, stop = base),
                            removeColon(substr(x, base + 1, nchar(formatSample))))
                frm <- paste0(frm, "%z")
            }
        }
    }

    as.POSIXct(x, format = frm, tz = timezone)
}
