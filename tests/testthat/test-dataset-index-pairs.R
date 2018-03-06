
context("dataset-index-pairs")

test_that("indexing with matrix pairs works", {
    ds <- as.dataset(iris)

    i <- c(47, 5, 132, 10, 142, 143, 123)
    j <- c(1, 3, 3, 1, 2, 2, 3)

    x <- ds[cbind(i,j)]
    y <- mapply(function(i, j) ds[i,j,drop=TRUE], i, j, SIMPLIFY = FALSE)

    expect_equal(x, y)
})


test_that("indexing with matrix indices works", {
    ds <- as.dataset(iris)

    i <- c(47, 5, 132, NA, 10, 142, 143, 123)
    j <- c(1, 3, 3, NA, 1, 2, 2, 3)
    index <- cbind(i + nrow(ds) * (j - 1))

    x <- ds[index]
    y <- ds[cbind(i,j)]

    expect_equal(x, y)
})


test_that("indexing with logical matrix works", {
    ds <- as.dataset(iris)

    index <- matrix(FALSE, nrow(ds), ncol(ds))
    i <- c(47, 5, 132, 10, 142, 143, 123)
    j <- c(1, 3, 3, 1, 2, 2, 3)
    index[cbind(i, j)] <- TRUE

    x <- ds[index]
    y <- ds[cbind(which(as.logical(index)))]
    expect_equal(x, y)
})


test_that("indexing with logical mask works", {
    ds <- as.dataset(iris)

    index <- matrix(FALSE, nrow(ds), ncol(ds))
    i <- c(47, 5, 132, 10, 142, 143, 123)
    j <- c(1, 3, 3, 1, 2, 2, 3)
    index[cbind(i, j)] <- TRUE

    x <- ds[cbind(as.logical(index))]
    y <- ds[index]
    expect_equal(x, y)
})


test_that("matrix entry", {
    ds <- dataset(a = matrix(1:6 * 10, 3, 2))
    x <- ds[cbind(2, 1)]
    y <- list(c(20, 50))
    expect_equal(x, y)
})


test_that("matrix entry replace", {
    skip("not implemented")
    x <- dataset(a = matrix(1:6 * 10, 3, 2))
    x[cbind(2, 1)] <- list(c(17, 14))

    y <- dataset(a = cbind(c(10, 17, 30), c(40, 14, 60)))
    expect_equal(x, y)
})


test_that("matrix entry replace recycle", {
    skip("not implemented")
    x <- dataset(a = matrix(1:6 * 10, 3, 2))
    x[cbind(2, 1)] <- 100

    y <- dataset(a = cbind(c(10, 100, 30), c(40, 100, 60)))
    expect_equal(x, y)
})


test_that("matrix entry wrong replace number", {
    skip("not implemented")
    x <- dataset(a = matrix(1:6 * 10, 3, 2))
    expect_error(x[cbind(2, 1)] <- c(17, 24),
                 "number of values \\(2\\) must match number of entries to replace \\(1\\)")
})
