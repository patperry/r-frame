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
