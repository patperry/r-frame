context("dataset-as")

test_that("as.data.frame", {
    x <- as.dataset(mtcars)
    x <- as.data.frame(x)

    y <- mtcars
    rownames(y) <- NULL

    expect_equal(x, y)
})


test_that("as.data.frame, no names", {
    x <- as.dataset(mtcars)
    names(x) <- NULL

    y <- x
    names(y) <- character(length(x))
    expect_equal(as.data.frame(x), as.data.frame(y))
})


test_that("as.data.frame, optional", {
    x <- dataset(`#` = 1:4)
    expect_equal(names(as.data.frame(x, optional = FALSE)), "X.")
    expect_equal(names(as.data.frame(x, optional = TRUE)), "#")
})


test_that("as.matrix", {
    x <- as.dataset(mtcars)
    x <- as.matrix(x)

    y <- as.matrix(mtcars)
    rownames(y) <- NULL
    storage.mode(y) <- "list"
    expect_equal(x, y)
})


test_that("as.matrix with matrix column", {
    x <- dataset(mat = matrix(1:6, 3, 2))
    y <- matrix(list(c(1L, 4L), c(2L, 5L), c(3L, 6L)), 3, 1)
    colnames(y) <- "mat"
    expect_equal(as.matrix(x), y)
})
