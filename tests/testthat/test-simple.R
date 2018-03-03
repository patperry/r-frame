context("simple")


test_that("converts numeric NaN to NA", {
    x <- c(1, NA, 1.3, .Machine$integer.max + 1, 23, NaN, Inf)
    y <- c(1, NA, 1.3, .Machine$integer.max + 1, 23, NA, Inf)
    expect_equal(as.simple(x), y)
})


test_that("converts factor to character", {
    x <- factor(c("5", "4", "3", "2", "1"), c(1, 2, 5, 3, 4))
    expect_equal(as.simple(x), as.character(x))
})


test_that("converts to POSIXct, preserving time zone", {
    ct <- as.POSIXct("2017-12-25 17:23:45", tz = "America/Los_Angeles")
    lt <- as.POSIXlt(ct)
              
    expect_equal(ct, as.simple(ct))
    expect_equal(ct, as.simple(lt))
})


test_that("homogeneous list", {
    l <- list(1, 2, 17)
    expect_equal(as.simple(l), as.numeric(l))
})


test_that("homogeneous boxed list", {
    l <- list(list(1), list(2), list(17))
    expect_equal(as.simple(l), c(1, 2, 17))
})


test_that("heterogeneous list", {
    l <- list(1, c(1, 3), 1)
    expect_error(as.simple(l),
                 "cannot convert heterogeneous list to simple vector")
})
