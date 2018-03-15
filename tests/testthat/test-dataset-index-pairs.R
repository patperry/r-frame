
context("dataset-index-pairs")

test_that("indexing with matrix pairs works", {
    ds <- as.dataset(iris)

    i <- c(47, 5, 132, 10, 142, 143, 123)
    j <- c(1, 3, 3, 1, 2, 2, 3)

    x <- ds[cbind(i,j)]
    y <- mapply(function(i, j) ds[i, j, drop = TRUE],
                i, j, SIMPLIFY = FALSE)

    expect_equal(x, y)
})


test_that("indexing with matrix indices works", {
    ds <- as.dataset(iris)

    i <- c(47, 5, 132, 10, 142, 143, 123)
    j <- c(1, 3, 3, 1, 2, 2, 3)
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
    x <- dataset(a = matrix(1:6 * 10, 3, 2))
    x[cbind(2, 1)] <- list(c(17, 14))

    y <- dataset(a = cbind(c(10, 17, 30), c(40, 14, 60)))
    expect_equal(x, y)
})


test_that("matrix entry replace recycle", {
    x <- dataset(a = matrix(1:6 * 10, 3, 2))
    x[cbind(2, 1)] <- 100

    y <- dataset(a = cbind(c(10, 100, 30), c(40, 100, 60)))
    expect_equal(x, y)
})


test_that("matrix entry wrong replace number", {
    x <- dataset(a = matrix(1:6 * 10, 3, 2))
    expect_error(x[cbind(2, 1)] <- c(17, 24),
                 "number of values \\(2\\) must match number of entries to replace \\(1\\)")
})


test_that("pair selection errors", {
    x <- dataset(a = 1:5, b = letters[1:5])
    expect_error(x[rbind(TRUE, TRUE)], "subscript length is 2, should be 10")
    expect_error(x[matrix(FALSE, 2, 3)], "subscript dimensions are 2 x 3, should be 5 x 2")
    expect_error(x[cbind(1, 1, 1)], "invalid matrix subscript \\(3 columns\\)")
})


test_that("indexing with scalar logical works", {
    x <- dataset(a = 1:5, b = letters[1:5])
    expect_equal(x[cbind(TRUE)], c(as.list(1:5), as.list(letters[1:5])))
    expect_equal(x[cbind(FALSE)], list())
})


test_that("index empty dataset", {
    x <- dataset(a = integer())
    expect_equal(x[matrix(0, 0, 1)], list())
    expect_equal(x[matrix(0, 0, 2)], list())
})


test_that("errors for invalid pair", {
    x <- dataset(x = 1:3,
                 foo = data.frame(bar = 1:3, baz = c("a", "b", "c"),
                                  stringsAsFactors = FALSE))
    expect_error(x[cbind(c(1, 2), c(2, 3))] <- list(100, list(99, 88)),
                 "subscript row 2 is invalid \\(2, 3\\)")
})


test_that("index setting with matrix and NA index errors", {
    i <- c(47, 5, 132, NA, 10, 142, 143, 123)
    j <- c(1, 3, 3, 1, 2, 2, NA, 3)
    val <- runif(length(i))

    x <- as.dataset(iris)
    expect_error(x[cbind(i, j)] <- val,
                 "subscript row 4 is invalid \\(NA, 1\\)")
})


test_that("matrix entry", {
    x <- dataset(x = 1:3,
                 foo = data.frame(bar = 1:3, baz = c("a", "b", "c"),
                                  stringsAsFactors = FALSE))
    x[rbind(c(2, 1),
            c(3, 2))] <- list(100, list(99, 88))

    y <- dataset(x = c(1, 100, 3),
                 foo = data.frame(bar = c(1, 2, 99),
                                  baz = c("a", "b", "88"),
                                  stringsAsFactors = FALSE))
    expect_equal(x, y)
})


test_that("errors for invalid single", {
    x <- dataset(x = 1:3, y = 4:6)
    expect_error(x[rbind(1, 4, 7, 3)], "subscript entry 3 is invalid \\(7\\)")
})
