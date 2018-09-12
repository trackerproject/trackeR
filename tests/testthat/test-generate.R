context("implementation [generation of variable names, base units and thresholds]")

test_that("variable names are generated correctly", {
    g <- generate_variable_names()
    expect_true(all(c("time", "latitude", "longitude", "altitude",
                      "distance", "heart_rate", "speed", "cadence_running",
                      "cadence_cycling", "power", "temperature") %in% g$human_names))
})

test_that("variables are generated correctly", {
    g <- generate_units()
    expect_equal(g[g$variable == "longitude", "unit"], rep("degree", 3))
    expect_equal(g[g$variable == "latitude", "unit"], rep("degree", 3))
    expect_equal(g[g$variable == "altitude", "unit"], rep("m", 3))
    expect_equal(g[g$variable == "distance", "unit"], rep("m", 3))
    expect_equal(g[g$variable == "heart_rate", "unit"], rep("bpm", 3))
    expect_equal(g[g$variable == "speed", "unit"], rep("m_per_s", 3))
    expect_equal(g[g$variable == "cadence_running", "unit"], rep("steps_per_min", 1))
    expect_equal(g[g$variable == "cadence_cycling", "unit"], rep("rev_per_min", 1))
    expect_equal(g[g$variable == "power", "unit"], rep("W", 1))
    expect_equal(g[g$variable == "temperature", "unit"], rep("C", 3))
    expect_equal(g[g$variable == "pace", "unit"], rep("min_per_km", 3))
    expect_equal(g[g$variable == "duration", "unit"], rep("min", 3))
})

test_that("thresholds are generated correctly", {
    g <- generate_thresholds()
    expect_true(all(c("variable", "unit", "lower", "upper", "sport") %in% names(g)))
})


