
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
    expect_error(x[j], "numeric index cannot contain both negative and non-negative values")
})


test_that("indexing with out of bounds positive fails", {
    x <- as.dataset(mtcars)
    j <- c(5, 3, 2, 100, 1)
    expect_error(x[j], "bounds error: index is 100, maximum is 11")
})


test_that("indexing with NA fails", {
    x <- as.dataset(mtcars)
    j <- c(seq_along(mtcars)[-1], NA)
    expect_error(x[j], "numeric index cannot contain NA values")
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
        "mismatch: logical mask length is 10, object length is 11")
})
