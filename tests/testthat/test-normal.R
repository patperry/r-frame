context("normal")

test_that("NULL", {
    expect_equal(as.normal(NULL), NULL)
})


test_that("logical", {
    expect_equal(as.normal(c(TRUE, FALSE, NA)),
                 c(TRUE, FALSE, NA))
})

test_that("raw", {
    expect_equal(as.normal(as.raw(c(0, 1, 255))),
                 as.raw(c(0, 1, 255)))
})


test_that("integer", {
    expect_equal(as.normal(c(0L, 1L, NA)),
                 c(0L, 1L, NA))
})


test_that("double", {
    x <- c(0, -0, 1, NaN, NA, Inf)
    y <- c(0,  0, 1, NaN, NA, Inf)
    expect_true(identical(as.normal(x), y, FALSE))
})


test_that("numeric", {
    x <- c(0, -0, 1, NaN, NA, Inf)
    expect_true(identical(as.normal.double(x),
                          as.normal.numeric(x), FALSE))
})


test_that("complex", {
    x <- complex(re = c(NA, 0, -0),
                 im = c( 0, 0,  0))
    y <- complex(re = c(NA, 0, 0),
                 im = c(NA, 0, 0))

    expect_true(identical(Re(as.normal(x)), Re(y), FALSE))
    expect_true(identical(Im(as.normal(x)), Im(y), FALSE))
})


test_that("character", {
    x <- c("fa\u00E7ile", "fa\xE7ile")
    Encoding(x) <- c("UTF-8", "latin1")

    y <- rep(x[1], 2)
    expect_true(identical(as.normal(x), y))
    expect_true(identical(Encoding(as.normal(x)), Encoding(y)))
})
