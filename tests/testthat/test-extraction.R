context("implementation [extraction utilities]")

gpxfile_run <- system.file("extdata/gpx/", "20170708-154835-Run.gpx", package = "trackeR")
gpx <- readGPX(gpxfile_run)

test_that("resting_periods returns the expected number of splits", {
    rp1 <- resting_periods(gpx$time, 2)
    rp2 <- resting_periods(gpx$time, 0.5/60)
    expect_equal(nrow(rp1$sessions), 1)
    expect_equal(nrow(rp2$sessions), 3)
})

test_that("get_sessions splits the data correctly in multivariate zoo objects", {
    g1 <- get_sessions(gpx, 2)
    g2 <- get_sessions(gpx, 0.5/60)
    for (j in seq.int(length(g1))) {
        expect_is(g1[[j]], "zoo")
    }
    for (j in seq.int(length(g2))) {
        expect_is(g2[[j]], "zoo")
    }
    expect_true(all.equal(do.call("rbind", g2), g1[[1]], tolerance = 1e-12))
})

