context("implementation [reading tcx, gpx and jsonlite files]")

tcxfile <- system.file("extdata/tcx/", "2013-06-30-070511.TCX", package = "trackeR")
gpxfile_run <- system.file("extdata/gpx/", "20170708-154835-Run.gpx", package = "trackeR")
gpxfile_ride <- system.file("extdata/gpx/", "20170709-151453-Ride.gpx", package = "trackeR")
gpxfile_swim <- system.file("extdata/gpx/", "20170714-143644-Swim.gpx", package = "trackeR")
jsonfile <- system.file("extdata/json/", "2017_04_24_10_18_45.json", package = "trackeR")

## tcx
tcx <- readTCX(tcxfile)
test_that("tcx file is read correctly [result is a data.frame]", {
    expect_is(tcx, "data.frame")
})
test_that("tcx file is read correctly [time, gps, cadence and sport]", {
    expect_equal(attr(tcx, "sport"), "running")
    expect_true(all(is.na(tcx$cadence_cycling)))
    expect_false(all(is.na(tcx$cadence_running)))
    expect_is(tcx$time, "POSIXct")
    expect_false(all(is.na(tcx$latitude)))
    expect_false(all(is.na(tcx$longitude)))

})

## gpx
gpx_run <- readGPX(gpxfile_run)
gpx_ride <- readGPX(gpxfile_ride)
gpx_swim <- readGPX(gpxfile_swim)
test_that("gpx file is read correctly [result is a data.frame]", {
    expect_is(gpx_run, "data.frame")
    expect_is(gpx_ride, "data.frame")
    expect_is(gpx_swim, "data.frame")
})
test_that("gpx file is read correctly [time, gps, cadence and sport]", {
    expect_equal(attr(gpx_run, "sport"), "running")
    expect_equal(attr(gpx_swim, "sport"), "swimming")
    expect_equal(attr(gpx_ride, "sport"), "cycling")
    expect_is(gpx_run$time, "POSIXct")
    expect_false(all(is.na(gpx_run$latitude)))
    expect_false(all(is.na(gpx_run$longitude)))
    expect_is(gpx_ride$time, "POSIXct")
    expect_false(all(is.na(gpx_ride$latitude)))
    expect_false(all(is.na(gpx_ride$longitude)))
    expect_is(gpx_swim$time, "POSIXct")
    expect_false(all(is.na(gpx_swim$latitude)))
    expect_false(all(is.na(gpx_swim$longitude)))
    ## There is cadence in the cycling file
    expect_true(all(is.na(gpx_run$cadence_cycling)))
    expect_false(all(is.na(gpx_run$cadence_running)))
    ## No cadence in the ride file
    expect_true(all(is.na(gpx_ride$cadence_cycling)))
    expect_true(all(is.na(gpx_ride$cadence_running)))
    ## No cadence in the swim file
    expect_true(all(is.na(gpx_swim$cadence_cycling)))
    expect_true(all(is.na(gpx_swim$cadence_running)))
})

## json
test_that("json file is read correctly [result is a data.frame]", {
    expect_warning(json <- readJSON(jsonfile))
    expect_is(json, "data.frame")
})
test_that("json file is read correctly [time, gps, cadence and sport]", {
    expect_warning(json <- readJSON(jsonfile))
    expect_equal(attr(json, "sport"), "cycling")
    expect_false(all(is.na(json$cadence_cycling)))
    expect_true(all(is.na(json$cadence_running)))
    expect_true(all(is.na(json$latitude)))
    expect_true(all(is.na(json$longitude)))
    expect_is(json$time, "POSIXct")
})

