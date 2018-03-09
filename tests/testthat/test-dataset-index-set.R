context("dataset-index-set")

test_that("[[<-", {
    x <- dataset(a = 1:5, b = letters[1:5], c = rep(TRUE, 5))
    x[[2]] <- 6:10
    y <- dataset(a = 1:5, b = 6:10, c = rep(TRUE, 5))
    expect_equal(x, y)
})


test_that("[[<- wrong length", {
    x <- dataset(a = c(1, 2))
    expect_error(x[[2]] <- 1, "mismatch: replacement has 1 rows, should have 2")
})


test_that("[[<- matrix", {
    x <- dataset(a = c(1, 2))
    mat <- matrix(1:6, 2, 3)
    x[[2]] <- mat

    y <- as.dataset(as.record(list(a = c(1, 2), mat)))
    expect_equal(x, y)
})


test_that("[[<- array", {
    x <- dataset(a = 1)
    y <- array(2, c(1,1,1))
    expect_error(x[[2]] <- y, "replacement is not a vector or matrix")
})


test_that("$<-", {
    x <- dataset(a = c(1, 2))
    x$foo <- c("hello", "world")
    y <- dataset(a = c(1, 2), foo = c("hello", "world"))
    expect_equal(x, y)
})


test_that("deleting all columns", {
    x <- as.dataset(mtcars)
    x[] <- NULL
    y <- as.dataset(mtcars)
    for (i in seq_along(y))
        y[[i]] <- NULL
    expect_equal(x, y)
})


test_that("setting array errors", {
    x <- as.dataset(mtcars)
    col <- array(1, dim = c(1, 1, nrow(x)))
    expect_error(x[[1]] <- col,
                 "replacement is not a vector or matrix")
})


test_that("setting too short", {
    x <- as.dataset(mtcars)
    col <- c(1, 2)
    expect_error(x[[1]] <- col, "replacement has 2 rows, should have 32")
})


test_that("setting scalar", {
    x <- as.dataset(mtcars)
    expect_error(x[[1]] <- 1.20, "replacement has 1 rows, should have 32")
})


test_that("appending column", {
    x <- as.dataset(mtcars)
    col <- rnorm(nrow(x))
    x$foo <- col

    mtcars$foo <- col
    y <- as.dataset(mtcars)
    expect_equal(x, y)
})


test_that("appending column by index", {
    ds <- mtcars
    n <- length(ds)
    col <- rnorm(nrow(ds))

    x <- as.dataset(ds)
    x[[n + 1L]] <- col

    ds[[n + 1L]] <- col
    names(ds)[[n + 1L]] <- NA
    y <- as.dataset(ds)

    expect_equal(x, y)
})


test_that("appending column by index, too far", {
    ds <- mtcars
    n <- length(ds)
    col <- rnorm(nrow(ds))

    x <- as.dataset(ds)
    x[[n + 3L]] <- col

    y <- as.dataset(c.record(ds, list(NULL, NULL, col)))
    keys(y) <- keys(as.dataset(ds))
    expect_equal(x, y)
})


test_that("appending column with no names", {
    x <- as.dataset(mtcars)
    names(x) <- NULL
    col <- rnorm(nrow(x))
    x$foo <- col

    mtcars$foo <- col
    y <- as.dataset(mtcars)
    names(y) <- c(character(length(y) - 1), "foo")
    expect_equal(x, y)
})


test_that("replace column with scalar", {
    x <- as.dataset(mtcars)
    expect_error(x[[5]] <- 17, "replacement has 1 rows, should have 32")
})


test_that("replace column with scalar, double index", {
    x <- as.dataset(mtcars)
    y <- mtcars
    x[, 5] <- 17
    y[, 5] <- 17
    y <- as.dataset(y)
    expect_equal(x, y)
})


test_that("deleting columns with single index", {
    x <- as.dataset(mtcars)
    y <- as.dataset(mtcars)
    x[c(4, 2, 4)] <- NULL
    y[c(4, 2)] <- NULL # errors on duplicates
    expect_equal(x, as.dataset(y))
})


test_that("deleting columns with name", {
    x <- as.dataset(mtcars)
    y <- as.dataset(mtcars)
    i <- names(x)[c(5, 3, 8, 3)]
    x[i] <- NULL
    y[c(5, 3, 8, 3)] <- NULL
    expect_equal(x, y)
})


test_that("deleting columns with single index and comma", {
    x <- as.dataset(mtcars)
    expect_error(x[,c(1, 1, 5)] <- NULL,
                 "selection dimensions are 32 x 3, replacement size is 0")
})


test_that("index setting with matrix pairs works", {
    i <- c(47, 5, 132, 10, 142, 143, 123)
    j <- c(1, 3, 3, 1, 2, 2, 3)
    val <- runif(length(i))

    x <- as.dataset(iris)
    x[cbind(i, j)] <- val

    y <- iris
    y[cbind(i, j)] <- val
    y <- as.dataset(y)

    expect_equal(x, y)
})


test_that("index setting with matrix pairs recycles", {
    i <- c(47, 5, 132, 10, 142, 143, 123)
    j <- c(1, 3, 3, 1, 2, 2, 3)
    val <- 1.1

    x <- as.dataset(iris)
    x[cbind(i, j)] <- val

    y <- iris
    y[cbind(i, j)] <- val
    y <- as.dataset(y)

    expect_equal(x, y)
})


test_that("index setting with matrix and NA index errors", {
    i <- c(47, 5, 132, NA, 10, 142, 143, 123)
    j <- c(1, 3, 3, 1, 2, 2, NA, 3)
    val <- runif(length(i))

    x <- as.dataset(iris)
    expect_error(x[cbind(i, j)] <- val,
                 "NAs are not allowed in subscripted assignments")
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
    expect_error(x[rbind(1, 4, 7, 3)], "index 3 \\(7\\) is out of bounds")
})


test_that("errors for invalid pair", {
    x <- dataset(x = 1:3,
                 foo = data.frame(bar = 1:3, baz = c("a", "b", "c"),
                                  stringsAsFactors = FALSE))
    expect_error(x[cbind(c(1, 2), c(2, 3))] <- list(100, list(99, 88)),
                 "index 2 \\(2, 3\\) is out of bounds")
})


test_that("setting invalid errors", {
    x <- as.dataset(mtcars)
    expect_error(x[[17]] <- array(1, c(1,1,1)),
                 "replacement is not a vector or matrix")
})


test_that("setting cells", {
    ds <- mtcars
    mat <- matrix(1:20, 4, 5)
    x <- as.dataset(ds)
    i <- c(3, 9, 2, 1)
    j <- c(5, 3, 2, 7, 8)
    x[i, j] <- mat
    y <- ds
    y[i, j] <- mat
    y <- as.dataset(y)
    expect_equal(x, y)
})
