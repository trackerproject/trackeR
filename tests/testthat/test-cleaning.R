context("implementation [sanity checks, distance correction, imputation of speeds]")

tcxfile <- system.file("extdata/tcx/", "2013-06-30-070511.TCX", package = "trackeR")
gpxfile_ride <- system.file("extdata/gpx/", "20170709-151453-Ride.gpx", package = "trackeR")

tcx <- readTCX(tcxfile)
test_that("sanity_checks returns warning [silent = FALSE] and removes duplicates", {
    expect_warning(tcx_c <- sanity_checks(tcx, silent = FALSE))
    expect_true(any(duplicated(tcx$time)))
    expect_false(any(duplicated(tcx_c$time)))
})

gpx <- readGPX(gpxfile_ride)
test_that("distance correction works", {
    gpx_c <- distance_correction(gpx)
    expect_gt(max(na.omit(gpx_c$distance)), max(gpx$distance))
})


test_that("impute_speeds imputes speeds and imputes power [sport = 'cycling']", {
    s0 <- sanity_checks(gpx, silent = TRUE)
    s1 <- get_sessions(s0)
    s2 <- impute_speeds(s1[[1]], from_distances = TRUE, sport = "cycling")
    s3 <- impute_speeds(s1[[1]], from_distances = TRUE, sport = "running")
    expect_true(all(is.na(head(s1[[1]]$speed))))
    expect_true(all(!is.na(head(s2$speed))))
    expect_true(all(!is.na(head(s2$power))))
    expect_true(all(is.na(head(s3$power))))
})

test_that("impute_speeds imputes power ", {
    s0 <- sanity_checks(gpx, silent = TRUE)
    s1 <- get_sessions(s0)
    s2 <- impute_speeds(s1[[1]], from_distances = TRUE)
    expect_true(all(is.na(head(s1[[1]]$speed))))
    expect_true(all(!is.na(head(s2$speed))))
})

