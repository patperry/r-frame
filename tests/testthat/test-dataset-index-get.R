
context("index-dataset-index-get")

test_that("$ indexing requires exact", {
    x <- as.dataset(mtcars)
    expect_error(x$mp, "unknown name \"mp\"")
})


test_that("indexing with column number works", {
    x <- as.dataset(mtcars)
    j <- c(5, 3, 0, 7)
    expect_equal(x[j], as.dataset(mtcars[j]))
})


test_that("indexing with negative column number works", {
    x <- as.dataset(mtcars)
    j <- -c(5, 3, 7)
    expect_equal(x[j], as.dataset(mtcars[j]))
})


test_that("indexing with mixed sign fails", {
    x <- as.dataset(mtcars)
    j <- c(-5, -3, 7)
    expect_error(x[j], "numeric subscript cannot contain both negative and non-negative values")
})


test_that("indexing with out of bounds positive fails", {
    x <- as.dataset(mtcars)
    j <- c(5, 3, 2, 100, 1)
    expect_error(x[j], "bounds error: index is 100, maximum is 11")
})


test_that("indexing with NA fails", {
    x <- as.dataset(mtcars)
    j <- c(seq_along(mtcars)[-1], NA)
    expect_error(x[j], "numeric subscript cannot contain NA values")
})


test_that("indexing with column logical works", {
    x <- as.dataset(mtcars)
    j <- rep(c(TRUE, FALSE), length(mtcars))[seq_along(mtcars)]
    expect_equal(x[j], as.dataset(mtcars[j]))
})


test_that("indexing with NA column logical does not error", {
    x <- as.dataset(mtcars)
    j <- c(rep(TRUE, length(mtcars) - 1), NA)
    expect_equal(x[j], x[j %in% TRUE])
})


test_that("indexing with wrong number of logical errors", {
    x <- as.dataset(mtcars)
    j <- c(rep(TRUE, length(mtcars) - 1))
    expect_error(x[j],
        "mismatch: logical subscript length is 10, should be 11")
})


test_that("indexing with row number works", {
    x <- as.dataset(mtcars)
    i <- c(13, 5, 20, 19)
    expect_equal(x[i,], as.dataset(mtcars[i,]))
})


test_that("indexing with row number and 'drop' works", {
    x <- as.dataset(mtcars)
    i <- c(13, 5, 20, 19)
    expect_equal(x[i, , drop = FALSE], as.dataset(mtcars[i,]))
})



test_that("indexing with column number works", {
    x <- as.dataset(mtcars)
    j <- c(5, 3, 7)
    expect_equal(x[,j], as.dataset(mtcars[,j]))
})


test_that("indexing without keys works", {
    x <- dataset(a = letters)
    expect_equal(as.dataset(x[1:5, , drop = FALSE]),
                 dataset(a = letters[1:5]))
})


test_that("getting all columns", {
    x <- dataset(a = letters)
    expect_equal(x, x[])
    expect_equal(x[], x[NULL])
})


test_that("indexing with keys, duplicate row", {
    x <- dataset(a = letters[1:5])
    keys(x) <- dataset(k = 2 * (1:5))
    i <- c(1, 1, 2, 1)

    y <- dataset(a = letters[i])
    keys(y) <- dataset(k = (2 * (1:5))[i], `#` = c(1L, 2L, 1L, 3L))

    expect_equal(x[i, ], y)
})


test_that("indexing with keys, duplicate row, no names", {
    x <- dataset(a = letters[1:5])
    keys(x) <- as.record(list(2 * (1:5)))
    i <- c(1, 1, 2, 1)

    y <- dataset(a = letters[i])
    keys(y) <- as.record(list((2 * (1:5))[i], `#` = c(1L, 2L, 1L, 3L)))

    expect_equal(x[i, ], y)
})
