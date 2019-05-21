context("implementation [trackeRdata, read_container]")

tcxfile <- system.file("extdata/tcx/", "2013-06-30-070511.TCX.gz", package = "trackeR")
gpxfile <- system.file("extdata/gpx/", "20170709-151453-Ride.gpx.gz", package = "trackeR")
gpxfile_swim <- system.file("extdata/gpx/", "20170714-143644-Swim.gpx.gz", package = "trackeR")

tol <- .Machine$double.eps

tcx1 <- read_container(tcxfile, silent = TRUE)
tcx2 <- read_container(tcxfile, session_threshold = 0.5/60, silent = TRUE)
gpx4 <- read_container(gpxfile, type = "gpx", session_threshold = 0.5/60, silent = FALSE)
gpx1 <- read_container(gpxfile_swim, type = "gpx", session_threshold = 2, silent = FALSE)

test_that("class of object from read_container is trackeRdata", {
    ## read_container
    expect_is(tcx1, "trackeRdata")
    expect_is(tcx2, "trackeRdata")
    expect_is(gpx4, "trackeRdata")
})

test_that("trackeRdata extractor methods work [nsessions]", {
    expect_equal(nsessions(tcx1), 1, tolerance = tol)
    expect_equal(nsessions(tcx2), 2, tolerance = tol)
    expect_equal(nsessions(gpx4), 4, tolerance = tol)
})

test_that("trackeRdata extractor methods work [sport]", {
    expect_equal(get_sport(tcx1), "running")
    expect_equal(get_sport(tcx2), rep("running", 2))
    expect_equal(get_sport(gpx4), rep("cycling", 4))
    expect_equal(get_sport(gpx1), "swimming")
})

test_that("trackeRdata extractor methods work [session_duration, session_times]", {
    expect_equal(as.numeric(session_duration(tcx1)), 5642/60/60, tolerance = tol)
    expect_equal(as.numeric(session_duration(tcx2)), c(1640, 3970)/60/60, tolerance = tol)
    expect_equal(as.numeric(session_duration(gpx1)), 2095/60/60, tolerance = tol)
    expect_is(session_times(gpx4), "data.frame")
    expect_equal(nrow(session_times(gpx4)), 4)
    expect_equal(names(session_times(gpx4)), c("sessionStart", "sessionEnd"))
})

test_that("c.trackeRdata works as expected [c-ing]", {

    s_12 <- c(tcx1, tcx2)
    expect_true(identical(s_12[1], tcx1) )

    s_all <- c(s_12, gpx4, gpx1)
    expect_true(identical(s_all[1:3], s_12))

    expect_true(identical(s_all[4:7],  gpx4))



})
