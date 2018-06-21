context("implementation [sanity checks, distance correction, imputation of speeds]")

tcxfile <- system.file("extdata/tcx/", "2013-06-30-070511.TCX", package = "trackeR")

tcx <- readTCX(tcxfile)
test_that("sanity_checks returns warning [silent = FALSE] and removes duplicates", {
    expect_warning(tcx_c <- sanity_checks(tcx, silent = FALSE))
    expect_true(any(duplicated(tcx$time)))
    expect_false(any(duplicated(tcx_c$time)))
})

test_that("distance correction works", {
    gpx_c <- distance_correction(gpx)
    expect_gt(max(gpx_c$distance), max(gpx$distance))
})
