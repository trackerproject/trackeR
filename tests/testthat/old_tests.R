context("implementation [extraction utilities]")

gpxfile_run <- system.file("extdata/gpx/", "20170708-154835-Run.gpx", package = "trackeR")
gpx <- readGPX(gpxfile_run)

test_that("resting_periods returns the expected number of splits", {
    rp1 <- resting_periods(gpx$time, session_threshold = 2)
    rp2 <- resting_periods(gpx$time, session_threshold = 0.5/60)
    expect_equal(nrow(rp1$sessions), 1)
    expect_equal(nrow(rp2$sessions), 3)
})

test_that("get_sessions returns a list of zoo objects that are the same when combined", {
    sess1 <- get_sessions(gpx, session_threshold = 2)
    sess2 <- get_sessions(gpx, session_threshold = 0.5/60)
    for (j in 1:length(sess1)) {
        expect_is(sess1[[j]], "zoo")
    }
    for (j in 1:length(sess2)) {
        expect_is(sess2[[j]], "zoo")
    }
    expect_true(all.equal(do.call("rbind", sess2), sess1[[1]], tolerance = 1e-15))
})
