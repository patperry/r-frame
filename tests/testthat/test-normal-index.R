
context("normal-index")


test_that("subset", {
    x <- as.normal(letters)
    i <- 5:10
    y <- as.normal(letters[i])
    expect_equal(x[i], y)
})


test_that("extract character", {
    x <- "1990-10-31"
    y <- as.normal(x)
    expect_equal(y[[1]], x)
})


test_that("extract Date", {
    x <- as.Date("1990-10-31")
    y <- as.normal(x)
    expect_equal(y[[1]], x)
})


test_that("extract POSIXct", {
    x <- as.POSIXct("2017-12-25 17:23:45", "America/Los_Angeles")
    y <- as.normal(x)
    expect_equal(y[[1]], x)
})


test_that("set item, character", {
    x <- as.normal("1990-10-31")
    x[[1]] <- "hello"
    expect_equal(x, "hello")
})


test_that("set item, Date", {
    x <- as.normal(as.Date("1990-10-31"))
    x[[1]] <- as.Date("1900-01-01")
    expect_equal(x, as.Date("1900-01-01"))
})


test_that("set item, POSIXct", {
    x <- as.POSIXct("2017-12-25 17:23:45", "America/Los_Angeles")
    y <- as.normal(x)
    y[[1]] <- as.POSIXct("2000-12-25 17:23:45", "America/Los_Angeles")
    expect_equal(y, as.POSIXct("2000-12-25 17:23:45", "America/Los_Angeles"))
})


test_that("set subset, character", {
    x <- as.normal(letters)
    x[2:3] <- c("hello", "world")
    expect_equal(x, as.character(x))
})


test_that("set subset, Date", {
    x <- as.Date(c("1000-01-01", "1500-01-01", "2000-01-01"))
    y <- as.normal(x)
    y[2] <- as.Date("1600-01-01")
    expect_equal(y, as.Date(c("1000-01-01", "1600-01-01", "2000-01-01")))
})
