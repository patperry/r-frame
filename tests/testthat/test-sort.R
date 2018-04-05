context("sort")

test_that("single column", {
    set.seed(0)
    x <- dataset(foo = rnorm(10))
    y <- x[order(x$foo), ]
    expect_equal(sort(x), y)
})


test_that("single column desc", {
    set.seed(0)
    x <- dataset(foo = rnorm(10))
    y <- x[order(x$foo, decreasing = TRUE), ]
    expect_equal(sort(x, decreasing = TRUE), y)
})


test_that("two columns", {
    set.seed(0)
    x <- dataset(foo = c(5:1, 1:5),
                 bar = rnorm(10))
    y <- x[order(x$foo, x$bar), ]
    expect_equal(sort(x), y)
})


test_that("two columns desc", {
    set.seed(0)
    x <- dataset(foo = c(5:1, 1:5),
                 bar = rnorm(10))
    y <- x[order(x$foo, x$bar, decreasing = TRUE), ]
    expect_equal(sort(x, decreasing = TRUE), y)
})


test_that("two columns + NULL", {
    set.seed(0)
    x <- dataset(foo = c(5:1, 1:5),
                 baz = NULL,
                 bar = rnorm(10))
    y <- x[order(x$foo, x$bar), ]
    expect_equal(sort(x), y)
})


test_that("list column", {
    x <- dataset(foo = as.list(letters))
    expect_error(xtfrm(x), "argument has a list column")
    expect_error(sort(x), "argument has a list column")
})


test_that("no columns", {
    x <- dataset(foo = letters)[0]
    expect_equal(xtfrm(x), 1:26)
    expect_equal(sort(x), x)
})


test_that("matrix column", {
    set.seed(0)
    mat <- matrix(runif(10), 5, 2)
    x <- dataset(mat)
    expect_equal(sort(x), x[order(mat[, 1], mat[, 2]), ])
})
