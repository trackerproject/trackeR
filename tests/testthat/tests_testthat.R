library(testthat)
context("Tests for tracker")

tcxfile <- system.file("extdata", "2013-06-08-090442.TCX", package = "trackeR")

DataNonGarmin <- readContainer(tcxfile, cores = 2)

## Test trackeRdata object
test_that("class of object from readContainer is trackeRdata", {
    expect_is(DataNonGarmin, "trackeRdata")
})

test_that("number of sessions in DataNonGarmin is 1", {
    expect_equal(length(DataNonGarmin), 1)
})

trackeRdatanames <- c("latitude", "longitude", "altitude", "distance", "heart.rate", "speed", "cadence", "power", "pace")
test_that("the names of each element of an trackeRdata object are as in trackeRdatanames", {
    expect_named(DataNonGarmin[[1]], trackeRdatanames)
})

test_that("class of each element of an trackeRdata object is of class zoo", {
    expect_is(DataNonGarmin[[1]], "zoo")
})


## Smoother
DataNonGarmin_smoothed <- smoother(DataNonGarmin, width = 20, what = "speed", cores = 2)

test_that("class of object from smoother.trackeRdata is trackeRdata", {
    expect_is(DataNonGarmin_smoothed, "trackeRdata")
})

test_that("only speed is smoothed in DataNonGarmin_smoothed (test only first session)", {
    Data_smoothed <- DataNonGarmin_smoothed[[1]]
    Data_original <- DataNonGarmin[[1]]
    inds <- match("speed", names(Data_smoothed))
    expect_equal(Data_smoothed[, -inds],
                 Data_original[index(Data_smoothed), -inds])
    expect_false(isTRUE(all.equal(Data_smoothed[, inds], Data_original[index(Data_smoothed), inds])))
})

test_that("smoother returns error is the trackeRdata object is already smoothed", {
    expect_error(smoother(DataNonGarmin_smoothed, cores = 2))
})


## Summary
DataNonGarmin_summary <- summary(DataNonGarmin)

test_that("standard keywords are produced from print.spdataSummary", {
    expect_output(print(DataNonGarmin_summary), "Session")
    expect_output(print(DataNonGarmin_summary), "Duration")
    expect_output(print(DataNonGarmin_summary), "Distance")
    expect_output(print(DataNonGarmin_summary), "speed")
    expect_output(print(DataNonGarmin_summary), "pace")
    expect_output(print(DataNonGarmin_summary), "time")
})


test_that("class of the object from summary.trackeRdata is trackeRdataSummary", {
    expect_is(DataNonGarmin_summary, "trackeRdataSummary")
})

test_that("object from summary.trackeRdata also inherit from data.frame", {
    expect_is(DataNonGarmin_summary, "data.frame")
})
