context("dataset-index")

test_that("[[<-", {
    x <- dataset(a = 1:5, b = letters[1:5], c = rep(TRUE, 5))
    x[[2]] <- 6:10
    y <- dataset(a = 1:5, b = 6:10, c = rep(TRUE, 5))
    expect_equal(x, y)
})


test_that("[[<- wrong length", {
    x <- dataset(a = c(1, 2))
    expect_error(x[[2]] <- 1, "mismatch: replacement has 1 rows, data has 2")
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
