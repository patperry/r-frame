context("dataset-index-drop")

test_that("drop = TRUE works for single column", {
    x <- as.dataset(mtcars)[, 1, drop = TRUE]
    y <- mtcars[, 1, drop = TRUE]
    expect_equal(x, y)
})


test_that("drop = TRUE works for single row", {
    x <- as.dataset(mtcars)[1, , drop = TRUE]
    y <- mtcars[1, , drop = TRUE]
    expect_equal(x, y)
})


test_that("drop = TRUE works for single element ", {
    x <- as.dataset(mtcars)[1, 1, drop = TRUE]
    y <- mtcars[1, 1, drop = TRUE]
    expect_equal(x, y)
})


test_that("drop = TRUE row works for matrix column", {
    ds <- dataset(group = mtcars)
    x <- ds[7, , drop = TRUE]
    y <- list(group = mtcars[7, , drop = TRUE])
    expect_equal(x, y)
})


test_that("drop = TRUE has no effect for empty selection", {
    ds <- as.dataset(mtcars[1, ])
    x <- ds[, , drop = TRUE]
    y <- ds
    expect_equal(x, y)
})


test_that("drop = TRUE with matrix col has no effect for empty", {
    ds <- dataset(group = mtcars[1, ])
    x <- ds[, , drop = TRUE]
    y <- ds
    expect_equal(x, y)
})


test_that("drop = TRUE ignores missing arguments", {
    x <- dataset(foo = "bar")[1, , drop = TRUE]
    y <- list(foo = "bar")
    expect_equal(x, y)
})


test_that("drop = TRUE with matrix works", {
    ds <- dataset(foo = matrix(4:6, 1, 3,
                               dimnames = list(NULL, c("a", "b", "c"))))
    x <- ds[1, , drop = TRUE]
    y <- list(foo = c(a = 4, b = 5, c = 6))
    expect_equal(x, y)
})


test_that("drop = TRUE with data.frame works", {
    ds <- dataset(foo = data.frame(a = 4, b = 5, c = 6))
    x <- ds[1, , drop = TRUE]
    y <- list(foo = list(a = 4, b = 5, c = 6))
    expect_equal(x, y)
})


test_that("drop = TRUE with data.frame single column works", {
    ds <- dataset(foo = data.frame(a = 4))
    x <- ds[1, , drop = TRUE]
    y <- list(foo = list(a = 4))
    expect_equal(x, y)
})


test_that("drop = TRUE with data.frame no column works", {
    ds <- dataset(foo = structure(list(), row.names = .set_row_names(1),
                                  class = "data.frame"))
    x <- ds[1, , drop = TRUE]
    y <- list(foo = list())
    expect_equal(x, y)
})
