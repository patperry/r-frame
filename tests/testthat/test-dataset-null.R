context("dataset-null")

test_that("allowed in dataset", {
    l <- list(x = 1:5, y = NULL, z = letters[1:5])
    x <- as.dataset(as.record(l))
    expect_equal(length(x), 3)
    expect_equal(dim(x), c(5, 3))
    expect_equal(names(x), c("x", "y", "z"))
    expect_equal(as.list(x), l)
})


test_that("can be converted from list", {
    x <- as.dataset(record(x = 1:5, y = NULL, z = letters[1:5]))
    y <- dataset(x = 1:5, y = NULL, z = letters[1:5])
    expect_equal(x, y)
})


test_that("can be retrieved with [[", {
    x <- dataset(x = 1:5, y = NULL, z = letters[1:5])
    expect_equal(x[[2]], NULL)
    expect_equal(x[["y"]], NULL)
})


test_that("can be retrieved with [ col", {
    x <- dataset(x = 1:5, y = NULL, z = letters[1:5])
    expect_equal(x[, 2, drop = TRUE], NULL)
    expect_equal(x[, "y", drop = TRUE], NULL)
})


test_that("can be retrieved with [ row", {
    x <- dataset(x = 1:5, y = NULL, z = letters[1:5])
    expect_equal(x[2, , drop = TRUE], record(x = 2, y = NULL, z = "b"))
})


test_that("setting scalar value with [[<-", {
    x <- dataset(x = 1:5, y = NULL, z = letters[1:5])
    x[[2]] <- rep(20, 5)
    y <- dataset(x = 1:5, y = rep(20, 5), z = letters[1:5])
    expect_equal(x, y)
})


test_that("setting scalar entry with [<-", {
    x <- dataset(x = 1:5, y = NULL, z = letters[1:5])
    x[4, 2] <- 20
    y <- dataset(x = 1:5,
                 y = list(NULL, NULL, NULL, 20, NULL),
                 z = letters[1:5])
    expect_equal(x, y)
})


test_that("setting vector entry with [<-", {
    x <- dataset(x = 1:5, y = NULL, z = letters[1:5])
    x[4, 2] <- list(c(20, 8))
    y <- dataset(x = 1:5,
                 y = list(NULL, NULL, NULL, c(20,  8), NULL),
                 z = letters[1:5])
    expect_equal(x, y)
})


test_that("setting NULL value with [<-", {
    x <- dataset(x = 1:5, y = NULL, z = letters[1:5])
    x[4, 2] <- list(NULL)
    y <- dataset(x = 1:5,
                 y = list(NULL, NULL, NULL, NULL, NULL),
                 z = letters[1:5])
    expect_equal(x, y)
})


test_that("printing", {
    x <- dataset(x = 1:5, y = NULL, z = letters[1:5])
    lines <- c(
'  x y    z',
'1 1 NULL a',
'2 2 NULL b',
'3 3 NULL c',
'4 4 NULL d',
'5 5 NULL e')

    expect_equal(strsplit(capture_output(print(x)), "\n")[[1]], lines)
})
