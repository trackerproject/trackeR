context("implementation [zones, change_units]")

data("runs", package = "trackeR")

tol <- .Machine$double.eps

test_that("zones method for trackeRdata objects [class, methods, output]", {
    expect_warning(z <- zones(runs, what = c("speed", "pace", "cadence_running", "heart_rate", "cadence_cyling", "asd"),
                                             unit_reference_sport = "cycling"),
                   regexp = "asd|cadence_cycling")
    expect_true(all(c("speed", "pace", "cadence_running", "heart_rate") %in% names(z)))
    expect_true(inherits(z, "trackeRdataZones"))
    expect_equal(nsessions(z), 27, tolerance = tol)
    expect_equal(dim(get_units(z)), c(30, 3), tolerance = tol)
    expect_equal(attr(z, "unit_reference_sport"), "cycling", tolerance = tol)
})
