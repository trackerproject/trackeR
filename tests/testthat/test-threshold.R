context("implementation [threshold-ing of trackeRdata obejcts]")


data("runs", package = "trackeR")

test_that("thresholds are generated correctly [defaults and user-supplied bounds]", {
    expect_error(generate_thresholds("speed"))
    expect_error(generate_thresholds(lower = c(0, 2)))
    g0 <- generate_thresholds()
    g <- generate_thresholds(variable = c("speed", "altitude"),
                             lower = c(0, 0),
                             upper = c(2, 2),
                             sport = c("swimming", "cycling"))
    inds1 <- g$sport == "cycling" & g$variable == "altitude"
    inds2 <- g$sport == "swimming" & g$variable == "speed"
    expect_equal(g[inds1, "lower"], 0)
    expect_equal(g[inds1, "upper"], 2)
    expect_equal(g[inds2, "lower"], 0)
    expect_equal(g[inds2, "upper"], 2)
    expect_equal(g0[!(inds1 | inds2), ], g[!(inds1 | inds2), ])
})

test_that("thresholds are applied correctly [speed, altitude]", {
    runs_th <- threshold(runs, variable = "speed", lower = 1, upper = 3, sport = "running")
    expect_true(all(sapply(runs_th, function(x) all(x[, "speed"] >= 1 & x[, "speed"] <= 3, na.rm = TRUE))))
    runs_th <- threshold(runs, variable = c("speed", "altitude"), lower = c(1, 10), upper = c(3, 20), sport = c("running", "running"))
    expect_true(all(sapply(runs_th, function(x) all(x[, "speed"] >= 1 & x[, "speed"] <= 3, na.rm = TRUE))))
    expect_true(all(sapply(runs_th, function(x) all(x[, "altitude"] >= 10 & x[, "altitude"] <= 20, na.rm = TRUE))))
})

test_that("change_units works with threshods", {
    runs_th <- threshold(runs, variable = "speed", lower = 1, upper = 3, sport = "running")
    r <- change_units(runs_th, variable = "speed", unit = "km_per_h", sport = "running")
    for (j in seq_along(r)) {
        expect_equal(runs_th[[j]][, "speed"]/1000 * 60 * 60, r[[j]][, "speed"])
    }
})


test_that("threshold works with all vairables", {
    th <- generate_thresholds()
    threshold(runs, th$variable, th$lower, th$upper, th$sport)
})


