context("implementation [distribution_profile, class, extractors, methods]")

test_that("disitrbution_profile works as expected [computation and methods]", {

    gpx <- system.file("extdata/gpx/", package = "trackeR")
    obj <- read_directory(gpx, silent = TRUE, verbose = FALSE)

    vars <- c(generate_variable_names()$human_names, "pace")
    expect_warning(dp <- distribution_profile(obj, what = vars), "no data|time|heart_rate|cadence_cycling")
    sm_dp <- smoother(dp, what = c("speed", "pace"))
    sc_dp <- scaled(dp, what = c("speed", "pace"))

    expect_true(all(names(dp) %in% vars))
    expect_equal(names(sm_dp), c("speed", "pace"))
    expect_equal(names(sm_dp), c("speed", "pace"))

    expect_equal(get_sport(dp), c("running", "cycling", "swimming"))
    expect_equal(get_sport(sm_dp), c("running", "cycling", "swimming"))
    expect_equal(get_sport(dp), c("running", "cycling", "swimming"))

    expect_equal(get_operations(sm_dp)$smooth, list(k = 30, sp = NULL, parallel = FALSE))
    expect_true(get_operations(sc_dp)$scale)

    expect_equal(attr(dp, "unit_reference_sport"), "cycling")
    expect_equal(attr(sm_dp, "unit_reference_sport"), "cycling")
    expect_equal(attr(sc_dp, "unit_reference_sport"), "cycling")

    expect_equal(attr(dp, "session_times"), session_times(obj))
    expect_equal(attr(sm_dp, "session_times"), session_times(obj))
    expect_equal(attr(sc_dp, "session_times"), session_times(obj))


    expect_equal(unclass(dp$speed[,1]*60), sapply(index(dp$speed), function(th) {
        timeAboveThreshold(obj[[1]][, "speed"], threshold = th, ge = FALSE)
    }), check.attributes = FALSE)

    ## subset
    dps <- get_profile(dp, session = c(1, 3), what = c("speed", "pace"))

    expect_equal(colnames(dps$speed), c("session1", "session3"))
    expect_equal(names(dps), c("speed", "pace"))

    att <- attributes(dp)
    atts <- attributes(dps)

    expect_equal(att$unit_reference_sport, atts$unit_reference_sport)
    expect_equal(att$operations, atts$operations)
    expect_equal(att$units, atts$units)
    expect_equal(att$sport[-2], atts$sport)
    expect_equal(att$session_times[-2, ], atts$session_times)

    expect_equal(class(dp), "distrProfile")
    expect_equal(class(sm_dp), "distrProfile")
    expect_equal(class(sc_dp), "distrProfile")
    expect_equal(class(dps), "distrProfile")

})

