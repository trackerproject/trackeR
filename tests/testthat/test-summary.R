context("implementation [summary, change_units, timeline]")


data("runs", package = "trackeR")

tol <- .Machine$double.eps


test_that("summary method for trackeRdata objects [class, methods, output]", {
    summ <- summary(runs)
    expect_true(inherits(summ, "trackeRdataSummary"))
    expect_equal(nsessions(summ), 27, tolerance = tol)
    expect_true(as.numeric(sum(session_duration(summ))) > 1000)
    expect_equal(dim(get_units(summ)), c(30, 3), tolerance = tol)
    expect_equal(attr(summ, "moving_threshold"), c(cycling = 2, running = 1, swimming = 0.5), tolerance = tol)
    expect_equal(attr(summ, "unit_reference_sport"), "cycling", tolerance = tol)
    expect_output(print(ss), "Session|Distance|Duration|Moving time|Average speed|Average speed moving|Work to rest ratio|Moving thresholds")
})
 
test_that("summary method for trackeRdata objects [change_units]", {
    summ0 <- summary(runs)
    summ1 <- summary(change_units(runs, c("speed", "distance"), c("mi_per_h", "mi"), c("cycling", "cycling")),
                     unit_reference_sport = "cycling")
    summ2 <- change_units(summ0, c("speed", "distance"), c("mi_per_h", "mi"))
    expect_equal(summ1, summ2, tolerance = tol)
    for (j in seq_along(summ0)) {
        expect_equal(m2mi(as.data.frame(summ0)[, "distance"]), as.data.frame(summ1)[, "distance"])
        expect_equal(m_per_s2mi_per_h(as.data.frame(summ0)[, "avgSpeed"]), as.data.frame(summ1)[, "avgSpeed"])
        expect_equal(m_per_s2mi_per_h(as.data.frame(summ0)[, "avgSpeedMoving"]), as.data.frame(summ1)[, "avgSpeedMoving"])
    }
})
