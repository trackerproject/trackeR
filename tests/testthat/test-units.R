context("implementation of units [generation, change]")

test_that("generate units returns the right units [defaults]", {
    un <- generate_units()
    expect_equal(dim(subset(un, sport == "running" & variable == "cadence_cycling")), c(0, 3))
    run <-  c("latitude", "longitude", "altitude", "distance", "heart_rate", "speed",
              "cadence_running", "temperature", "pace", "duration")
    expect_true(all(subset(un, sport == "running")$variable %in% run))
    expect_equal(dim(subset(un, sport == "cycling" & variable == "cadence_running")), c(0, 3))
    expect_equal(dim(subset(un, sport == "cycling" & variable == "cadence_cycling")), c(1, 3))
})

test_that("generate units returns correct units [user specified]", {
    expect_error(generate_units(variable = "cadence", sport = "cycling", unit = "rev_per_min"))
    expect_error(generate_units(variable = "cadence_cycling", sport = "running", unit = "rev_per_min"))
    un <- generate_units(variable = c("speed", "altitude", "distance"),
                         sport = c("cycling", "running", "cycling"),
                         unit =  c("mi_per_h", "km", "ft"))
    expect_equal(subset(un, sport == "cycling" & variable == "speed")$unit, "mi_per_h")
    expect_equal(subset(un, sport == "cycling" & variable == "distance")$unit, "ft")
    expect_equal(subset(un, sport == "running" & variable == "altitude")$unit, "km")
})

gpxfile_run <- system.file("extdata/gpx/", "20170708-154835-Run.gpx", package = "trackeR")
gpxfile_ride <- system.file("extdata/gpx/", "20170709-151453-Ride.gpx", package = "trackeR")
test_that("change units works as expected []", {
    gpxa <- read_container(gpxfile_run, type = "gpx")
    gpxb <- read_container(gpxfile_ride, type = "gpx")
    gpxa1 <- change_units(gpxa,
                          variable = "speed",
                          unit = "mi_per_h",
                          sport = "cycling")
    expect_equal(gpxa1, gpxa, check.attributes = FALSE)

    gpxb1 <- change_units(gpxb,
                          variable = c("speed", "distance", "temperature"),
                          unit = c("mi_per_h", "km", "F"),
                          sport = c("cycling", "cycling", "running"))
    expect_equal(gpxb1[[1]]$speed, gpxb[[1]]$speed/1609.344 * 60 * 60)
    expect_equal(gpxb1[[1]]$distance, gpxb[[1]]$distance/1000)
    u <- getUnits(gpxb1)
    expect_equal(u[u$variable == "temperature" & u$sport == "cycling", "unit"], "C")

})

