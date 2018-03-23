
context("unique")

test_that("logical", {
    x <- dataset(col = c(TRUE, FALSE, NA, FALSE, TRUE, NA))
    expect_equal(anyDuplicated(x), anyDuplicated(x$col))
    expect_equal(duplicated(x), duplicated(x$col))
    expect_equal(unique(x), dataset(col = unique(x$col)))
})


test_that("raw", {
    x <- dataset(col = as.raw(c(0, 1, 1, 3, 1, 255)))
    expect_equal(anyDuplicated(x), anyDuplicated(x$col))
    expect_equal(duplicated(x), duplicated(x$col))
    expect_equal(unique(x), dataset(col = unique(x$col)))
})


test_that("integer", {
    x <- dataset(col = as.integer(c(-99, NA, -99, 8, 8, 0, 1, 1, 3, 1, 0,
                                    77, 255, NA, 12, 13, 12, -1, NA, -1)))
    expect_equal(anyDuplicated(x), anyDuplicated(x$col))
    expect_equal(duplicated(x), duplicated(x$col))
    expect_equal(unique(x), dataset(col = unique(x$col)))
})


test_that("double", {
    x <- dataset(col = c(0.42, NA, -0.06, NaN, 0.42, NA, 0.42, NA,
                         NA, -0.06, 0.42, 0.27, 0.81, Inf, Inf, 0.27, 0.42,
                         0.27, 0.27, 0.13, -0.06, -0.06, NaN, -Inf,
                         0.15, 0.1 + 0.05))
    expect_equal(anyDuplicated(x), anyDuplicated(x$col))
    expect_equal(duplicated(x), duplicated(x$col))
    expect_equal(unique(x), dataset(col = unique(x$col)))
})


test_that("complex", {
    set.seed(0)
    x <- dataset(col = (sample(c(NA, rnorm(5)), 50, replace = TRUE)
                        + 1i * sample(c(NA, rnorm(4)), 50, replace = TRUE)))
    y <- dataset(re = Re(x$col), im = Im(x$col))
    expect_equal(anyDuplicated(x), anyDuplicated(y))
    expect_equal(duplicated(x), duplicated(y))
    expect_equal(unique(x), x[!duplicated(x), ])
})


test_that("character", {
    set.seed(0)

    set <- c(letters)
    for (i in 1:4) {
        set <- c(set, paste0(set, set))
    }

    x <- dataset(col = sample(c("", NA, set), 1000, replace = TRUE))
    expect_equal(anyDuplicated(x), anyDuplicated(x$col))
    expect_equal(duplicated(x), duplicated(x$col))
    expect_equal(unique(x), dataset(col = unique(x$col)))
})


test_that("large set", {
    set.seed(0)
    x <- dataset(col = sample.int(200, 1000, replace = TRUE))
    expect_equal(anyDuplicated(x), anyDuplicated(x$col))
    expect_equal(duplicated(x), duplicated(x$col))
    expect_equal(unique(x), dataset(col = unique(x$col)))
})