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
                    "power",
                    "temperature")

    ## resources for tcx:
    ## https://en.wikipedia.org/wiki/Training_Center_XML
    ## http://www8.garmin.com/xmlschemas/index.jsp#/web/docs/xmlschemas
    ## http://www.garmindeveloper.com/schemas/tcx/v2/
    tcxNames <- c("Time",
                  "LatitudeDegrees",
                  "LongitudeDegrees",
                  "AltitudeMeters",
                   "DistanceMeters",
                  "HeartRateBpm",
                  "Speed",
                  "Cadence",
                  "Watts",
                  "temperature")


    ## resources for tcx:
    ## https://en.wikipedia.org/wiki/Training_Center_XML
    ## http://www8.garmin.com/xmlschemas/index.jsp#/web/docs/xmlschemas
    ## http://www.garmindeveloper.com/schemas/tcx/v2/
    gpxNames <- c("time",
                  "lat",
                  "lon",
                  "ele",
                  "distance",
                  "hr",
                  "speed",
                  "cad",
                  "watts",
                  "atemp")

    ## Resource for db3: none... mostly reverse engineering
    db3Names <-     c("dttm",
                      "lat",
                      "lon",
                      "altitude",
                      "dist",
                      "hr",
                      "velocity",
                      "cadence",
                      "watts",
                      "temperature")

    ## Resource for Golden Cheetah JSON: reverse engineering
    jsonNames <- c("SECS",
                   "LAT",
                   "LON",
                   "ALT",
                   "KM",
                   "HR",
                   "KPH",
                   "CAD",
                   "WATTS",
                   "temperature")

    list(humanNames = humanNames,
         gpxNames = gpxNames,
         tcx2Names = tcxNames,
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
#'
#' @export
readTCX <- function(file, timezone = "", speedunit = "m_per_s", distanceunit = "m",
                    parallel = FALSE, cores = getOption("mc.cores", 2L),...) {

    doc <- read_xml(file)
    ns <- xml_ns(doc)

    children_names <- function(x, xpath, ns) {
        unique(xml_name(xml_children(xml_find_all(x, xpath, ns))))
    }

    ## Core namespaces
    activity_ns <- names(which(ns == "http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2")[1])
    ## https://www8.garmin.com/xmlschemas/ActivityExtensionv2.xsd
    ## https://www8.garmin.com/xmlschemas/ActivityExtensionv1.xsd
    extensions_ns <- c("http://www.garmin.com/xmlschemas/ActivityExtension/v2",
                       "http://www.garmin.com/xmlschemas/ActivityExtension/v1")
    extensions_ns <- na.omit(sapply(extensions_ns, function(e) names(which(ns == e)[1])))

    ## Sport
    sport <- xml_attr(xml_find_all(doc, paste0("//", activity_ns, ":", "Activity")), "Sport")

    ## Tp
    tp_xpath <- paste0("//", activity_ns, ":", "Trackpoint")
    tp_vars <- data.frame(name = children_names(doc, tp_xpath, ns),
                          ns = activity_ns)

    ## Position
    position_xpath <- paste0("//", activity_ns, ":", "Position")
    ## Add any nested fields here
    is_position <- tp_vars$name == "Position"
    if (any(is_position)) {
        ## remove position
        tp_vars <- tp_vars[!is_position, ]
        ## Add longitude/latitude
        children <- data.frame(name = children_names(doc, position_xpath, ns[activity_ns]),
                               ns = activity_ns)
        tp_vars <- rbind(tp_vars, children)
    }

    ## Extensions
    is_extensions <- tp_vars$name == "Extensions"
    if (any(is_extensions)) {
        ## remove position
        tp_vars <- tp_vars[!is_extensions, ]
        for (e in extensions_ns) {
            e_xpath <- paste0("//", e, ":", "TPX")
            ## Add any extensions
            ch_nam <- children_names(doc, e_xpath, ns[e])
            if (length(ch_nam)) {
                children <- data.frame(name = ch_nam, ns = e)
                tp_vars <- rbind(tp_vars, children)
            }
        }
    }

    is_time <- tp_vars$name == "Time"

    tps <- xml_find_all(doc, tp_xpath, ns[activity_ns])
    ## Double loop to extract obs
    observations <- apply(tp_vars, 1, function(var) {
        c_xpath <- paste0(".", "//", var["ns"], ":", var["name"])
        c_ns <- ns[var["ns"]]
        sapply(tps, function(x) {
            xml_text(xml_find_first(x, c_xpath, c_ns))
        })
    })

    observations <- as.data.frame(observations, stringsAsFactors = FALSE)
    ## Rename RunCadence to Cadence

    names(observations) <- tp_vars$name
    run_cadence <- tp_vars$name == "RunCadence"
    if (any(run_cadence)) {
        names(observations)[run_cadence] <- "Cadence"
    }
    observations[!is_time] <- apply(observations[!is_time], 2, as.numeric)

    ## human names
    allnames <- generateVariableNames()
    namesOfInterest <- allnames$tcx2Names
    namesToBeUsed <- allnames$humanNames
    inds <- match(namesOfInterest, names(observations), nomatch = 0)
    observations <- observations[inds]
    names(observations) <- namesToBeUsed[inds!=0]

    ## coerce time into POSIXct
    observations$time <- gsub("[\t\n]", "", observations$time)
    observations$time <- convertTCXTimes2POSIXct(observations$time, timezone = timezone)

    ## Add missing varibles
    missingVars <- namesToBeUsed[match(namesToBeUsed, names(observations), nomatch = 0) == 0]
    if (nrow(observations) > 0) {
        for (nn in missingVars) {
            observations[[nn]] <- NA
        }
    }

    ## convert speed from speedunit to m/s
    if (speedunit != "m_per_s") {
        speedConversion <- match.fun(paste(speedunit, "m_per_s", sep = "2"))
        observations$speed <- speedConversion(observations$speed)
    }

    ## convert distance from distanceunit to m
    if (distanceunit != "m") {
        distanceConversion <- match.fun(paste(distanceunit, "m", sep = "2"))
        observations$distance <- distanceConversion(observations$distance)
    }

    ## use variable order for trackeRdata
    if (any(names(observations) != allnames$humanNames)) {
        observations <- observations[, allnames$humanNames]
    }

    attr(observations, "sport") <- sport

    return(observations)

}

#' @inheritParams readX
#' @export
#' @rdname readX
readGPX <- function(file, timezone = "", speedunit = "km_per_h", distanceunit = "km",
                     parallel = FALSE, cores = getOption("mc.cores", 2L),...) {

    doc <- read_xml(file)
    ns <- xml_ns(doc)

    children_names <- function(x, xpath, ns) {
        unique(xml_name(xml_children(xml_find_all(x, xpath, ns))))
    }

    ## Core namespaces
    activity_ns <- names(which(ns == "http://www.topografix.com/GPX/1/1")[1])

    extensions_ns <- c("http://www.garmin.com/xmlschemas/TrackPointExtension/v1",
                       "http://www.garmin.com/xmlschemas/TrackPointExtension/v2",
                       "http://www.garmin.com/xmlschemas/GpxExtensions/v3")
    extensions_ns <- na.omit(sapply(extensions_ns, function(e) names(which(ns == e)[1])))


    ## Sport extraction
    sport <- xml_text(xml_find_first(doc, paste0("//", activity_ns, ":", "name")))
    keyword <- c("run", "cycl", "swim", "bik")
    sports <- c("Running", "Cycling", "Swimming", "Cycling")
    sport <- sports[sapply(keyword, function(key) grepl(key, sport, ignore.case = TRUE))]

    if (length(sport) == 0) {
        sport <- NA
    }

    ## Trackpoint
    tp_xpath <- paste0("//", activity_ns, ":", "trkpt")
    tp_vars <- data.frame(name = children_names(doc, tp_xpath, ns),
                          ns = activity_ns)

    is_extensions <- tp_vars$name == "extensions"
    if (any(is_extensions)) {
        ## remove position
        tp_vars <- tp_vars[!is_extensions, ]
        for (e in extensions_ns) {
            e_xpath <- paste0("//", e, ":", "TrackPointExtension")
            ## Add any extensions
            ch_nam <- children_names(doc, e_xpath, ns[e])
            if (length(ch_nam)) {
                children <- data.frame(name = ch_nam, ns = e)
                tp_vars <- rbind(tp_vars, children)
            }
        }
    }

    is_time <- tp_vars$name == "time"

    tps <- xml_find_all(doc, tp_xpath, ns[activity_ns])
    ## Double loop to extract obs
    observations <- apply(tp_vars, 1, function(var) {
        c_xpath <- paste0(".", "//", var["ns"], ":", var["name"])
        c_ns <- ns[var["ns"]]
        sapply(tps, function(x) {
            xml_text(xml_find_first(x, c_xpath, c_ns))
        })
    })

    observations <- as.data.frame(observations, stringsAsFactors = FALSE)

    names(observations) <- tp_vars$name

    observations[!is_time] <- apply(observations[!is_time], 2, as.numeric)

    ## Add lat and lon
    observations$lat <- as.numeric(xml_attr(tps, "lat", ns[activity_ns]))
    observations$lon <- as.numeric(xml_attr(tps, "lon", ns[activity_ns]))

    ## Compute distance
    observations$distance <- cumsum(c(0, sp::spDists(observations[, c("lon", "lat")], longlat = TRUE, segments = TRUE)))

    ## human names
    allnames <- generateVariableNames()
    namesOfInterest <- allnames$gpxNames
    namesToBeUsed <- allnames$humanNames
    inds <- match(namesOfInterest, names(observations), nomatch = 0)
    observations <- observations[inds]
    names(observations) <- namesToBeUsed[inds!=0]

    ## coerce time into POSIXct
    observations$time <- gsub("[\t\n]", "", observations$time)
    observations$time <- convertTCXTimes2POSIXct(observations$time, timezone = timezone)

    ## Add missing varibles
    missingVars <- namesToBeUsed[match(namesToBeUsed, names(observations), nomatch = 0) == 0]
    if (nrow(observations) > 0) {
        for (nn in missingVars) {
            observations[[nn]] <- NA
        }
    }

    ## convert speed from speedunit to m/s
    if (speedunit != "m_per_s") {
        speedConversion <- match.fun(paste(speedunit, "m_per_s", sep = "2"))
        observations$speed <- speedConversion(observations$speed)
    }

    ## convert distance from distanceunit to m
    if (distanceunit != "m") {
        distanceConversion <- match.fun(paste(distanceunit, "m", sep = "2"))
        observations$distance <- distanceConversion(observations$distance)
    }

    ## use variable order for trackeRdata
    if (any(names(observations) != allnames$humanNames)) {
        observations <- observations[, allnames$humanNames]
    }

    attr(observations, "sport") <- sport

    return(observations)

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
readJSON <- function(file, timezone = "", speedunit = "km_per_h", distanceunit = "km",
                     parallel = FALSE, cores = getOption("mc.cores", 2L), ...) {
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
