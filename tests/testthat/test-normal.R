context("normal")

test_that("NULL", {
    expect_equal(as.normal(NULL), NULL)
})


test_that("logical", {
    x <- c(TRUE, FALSE, NA)
    expect_equal(as.normal(x), x)
})


test_that("raw", {
    x <- as.raw(c(0, 1, 254, 255))
    expect_equal(as.normal(x), as.integer(x))
})


test_that("integer", {
    x <- c(-.Machine$integer.max, -1L, 0L, 1L, .Machine$integer.max, NA_integer_)
    expect_equal(as.normal(x), x)
    expect_true(is.integer(as.normal(x)))
})


test_that("hexmode", {
    x <- c(-.Machine$integer.max, -1L, 0L, 1L, .Machine$integer.max, NA_integer_)
    h <- as.hexmode(x)
    expect_equal(as.normal(h), x)
})


test_that("double", {
    x <- c(-Inf, -1, 1, NA, 1.3, .Machine$integer.max + 1, 23, NaN, Inf)
    y <- c(-Inf, -1, 1, NA, 1.3, .Machine$integer.max + 1, 23,  NA, Inf)
    expect_equal(as.normal(x), y)
})


test_that("complex", {
    x <- c(-1-1i, -1, 0, 1, 1i, 1i)
    y <- dataset(re = Re(x), im = Im(x))
    expect_equal(as.normal(x), y)
})


test_that("character", {
    x <- c("a", "zzzz", "", NA)
    y <- c("a", "zzzz", NA, NA)
    expect_equal(as.normal(x), y)
})


test_that("factor", {
    x <- factor(c("5", "4", "3", "2", "1"), c(1, 2, 5, 3, 4))
    expect_equal(as.normal(x), as.character(x))
})


test_that("Date", {
    x <- as.Date(c("1900-01-01", "1969-12-31", "1970-01-01", "1970-01-02", NA))
    expect_equal(as.normal(x), x)
})


test_that("POSIXct in native", {
    x <- as.POSIXct("2017-12-25 17:23:45")
    expect_equal(as.normal(x), x)
})


test_that("POSIXct in UTC", {
    x <- as.POSIXct("2017-12-25 17:23:45", "UTC")
    expect_equal(as.normal(x), x)
})


test_that("POSIXct in America/Los_Angeles", {
    x <- as.POSIXct("2017-12-25 17:23:45", "America/Los_Angeles")
    expect_equal(as.normal(x), x)
})


test_that("POSIXlt in native", {
    x <- as.POSIXlt("2017-12-25 17:23:45")
    y <- as.POSIXct("2017-12-25 17:23:45")
    expect_equal(as.normal(x), y)
})


test_that("POSIXlt in UTC", {
    x <- as.POSIXlt("2017-12-25 17:23:45", "UTC")
    y <- as.POSIXct("2017-12-25 17:23:45", "UTC")
    expect_equal(as.normal(x), y)
})


test_that("POSIXlt in America/Los_Angeles", {
    x <- as.POSIXlt("2017-12-25 17:23:45", "America/Los_Angeles")
    y <- as.POSIXct("2017-12-25 17:23:45", "America/Los_Angeles")
    expect_equal(as.normal(x), y)
})


test_that("list", {
    l <- list(1, 2, 17)
    expect_equal(as.normal(l), as.numeric(l))
})


test_that("list of boxed", {
    l <- list(list(1), list(2), list(17))
    expect_equal(as.normal(l), c(1, 2, 17))
})


test_that("list of heterogeneous", {
    l <- list(1, c(1, 3), 1)
    expect_error(as.normal(l),
                 "cannot convert heterogeneous list to normal")
})


test_that("matrix-like", {
    x <- data.frame(a = 1:5, b = letters[1:5], c = rep(TRUE, 5))
    y <- dataset(a = 1:5, b = letters[1:5], c = rep(TRUE, 5))
    expect_equal(as.normal(x), y)
})


test_that("record", {
    x <- record(a = 1, b = "foo", c = TRUE)
    y <- dataset(a = 1, b = "foo", c = TRUE)
    expect_equal(as.normal(x), y)
})


test_that("rank 3", {
    x <- array(1:24, c(2, 3, 4))
    expect_error(as.normal(x),
                 "cannot convert rank-3 objects to normal")
})


test_that("unknown class", {
    x <- structure(list(x = 1), class = "foo")
    expect_error(as.normal(x), "cannot convert objects of class \"foo\" to normal")
})


test_that("invalid mode", {
    expect_error(as.normal(sin), "cannot convert objects of mode \"function\" to normal")
})
