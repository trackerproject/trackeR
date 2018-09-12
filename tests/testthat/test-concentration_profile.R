context("implementation [distribution_profile, class, extractors, methods]")

test_that("concentration_profile works as expected [trackeRdata; computation and methods]", {

    gpx <- system.file("extdata/gpx/", package = "trackeR")
    obj <- read_directory(gpx, silent = TRUE, verbose = FALSE)

    vars <- c(generate_variable_names()$human_names, "pace")
    expect_warning(cp <- concentration_profile(obj, what = vars), "no data|time|heart_rate|cadence_cycling")
    expect_warning(sm_cp <- smoother(cp, what = c("speed", "pace")))

    expect_true(all(names(cp) %in% vars))
    expect_equal(names(sm_cp), c("speed", "pace"))

    expect_equal(get_sport(cp), c("running", "cycling", "swimming"))
    expect_equal(get_sport(sm_cp), c("running", "cycling", "swimming"))

    expect_equal(get_operations(sm_cp)$smooth, "density")
    expect_true(!get_operations(sm_cp)$scale)

    expect_equal(attr(cp, "unit_reference_sport"), "cycling")
    expect_equal(attr(sm_cp, "unit_reference_sport"), "cycling")

    expect_equal(attr(cp, "session_times"), session_times(obj))
    expect_equal(attr(sm_cp, "session_times"), session_times(obj))

    ## subset
    cps <- get_profile(cp, session = c(1, 3), what = c("speed", "pace"))

    expect_equal(colnames(cps$speed), c("session1", "session3"))
    expect_equal(names(cps), c("speed", "pace"))

    att <- attributes(cp)
    atts <- attributes(cps)

    expect_equal(att$unit_reference_sport, atts$unit_reference_sport)
    expect_equal(att$operations, atts$operations)
    expect_equal(att$units, atts$units)
    expect_equal(att$sport[-2], atts$sport)
    expect_equal(att$session_times[-2, ], atts$session_times)

    expect_equal(class(cp), "conProfile")
    expect_equal(class(sm_cp), "conProfile")

})


test_that("concentration_profile works as expected [distrProfile; computation and methods]", {

    gpx <- system.file("extdata/gpx/", package = "trackeR")
    obj <- read_directory(gpx, silent = TRUE, verbose = FALSE)

    vars <- c(generate_variable_names()$human_names, "pace")
    expect_warning(dp <- distribution_profile(obj, what = vars), "no data|time|heart_rate|cadence_cycling")
    sm_dp <- smoother(dp, what = c("speed", "pace"))
    sc_dp <- scaled(dp, what = c("speed", "pace"))

    cp <- concentration_profile(sm_dp)
    sm_cp <- concentration_profile(sm_dp)
    sc_cp <- concentration_profile(sc_dp)


    expect_true(all(names(cp) %in% vars))
    expect_equal(names(sm_cp), c("speed", "pace"))

    expect_equal(get_sport(cp), c("running", "cycling", "swimming"))
    expect_equal(get_sport(sm_cp), c("running", "cycling", "swimming"))

    expect_equal(get_operations(sm_cp)$smooth, list(k = 30, sp = NULL, parallel = FALSE))
    expect_true(!get_operations(sm_cp)$scale)

    expect_equal(attr(cp, "unit_reference_sport"), "cycling")
    expect_equal(attr(sm_cp, "unit_reference_sport"), "cycling")

    expect_equal(attr(cp, "session_times"), session_times(obj))
    expect_equal(attr(sm_cp, "session_times"), session_times(obj))

    ## subset
    cps <- get_profile(cp, session = c(1, 3), what = c("speed", "pace"))

    expect_equal(colnames(cps$speed), c("session1", "session3"))
    expect_equal(names(cps), c("speed", "pace"))

    att <- attributes(cp)
    atts <- attributes(cps)

    expect_equal(att$unit_reference_sport, atts$unit_reference_sport)
    expect_equal(att$operations, atts$operations)
    expect_equal(att$units, atts$units)
    expect_equal(att$sport[-2], atts$sport)
    expect_equal(att$session_times[-2, ], atts$session_times)

    expect_equal(class(cp), "conProfile")
    expect_equal(class(sm_cp), "conProfile")
    expect_equal(class(sc_cp), "conProfile")

})
