context("implementation [generation of variable names, base units and thresholds]")

test_that("variable names are generated correctly", {
    g <- generate_variable_names()
    expect_true(all(c("time", "latitude", "longitude", "altitude",
                      "distance", "heart_rate", "speed", "cadence_running",
                      "cadence_cycling", "power", "temperature") %in% g$human_names))
})

test_that("variables are generated correctly", {
    g <- generate_base_units()
    expect_equal(g[g$variable == "longitude", "unit"], "degree")
    expect_equal(g[g$variable == "latitude", "unit"], "degree")
    expect_equal(g[g$variable == "altitude", "unit"], "m")
    expect_equal(g[g$variable == "distance", "unit"], "m")
    expect_equal(g[g$variable == "heart_rate", "unit"], "bpm")
    expect_equal(g[g$variable == "speed", "unit"], "m_per_s")
    expect_equal(g[g$variable == "cadence_running", "unit"], "steps_per_min")
    expect_equal(g[g$variable == "cadence_cycling", "unit"], "rev_per_min")
    expect_equal(g[g$variable == "power", "unit"], "W")
    expect_equal(g[g$variable == "temperature", "unit"], "C")
    expect_equal(g[g$variable == "pace", "unit"], "min_per_km")
    expect_equal(g[g$variable == "duration", "unit"], "s")
})

test_that("thresholds are generated correctly", {
    g <- generate_thresholds()
    expect_true(all(c("variable", "unit", "lower", "upper", "sport") %in% names(g)))
})


