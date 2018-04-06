context("select")

test_that("no arguments", {
    x <- select(iris)
    y <- as.dataset(matrix(0, nrow(iris), 0))
    names(y) <- character(0)
    expect_equal(x, y)
})


test_that("named arguments", {
    x <- select(iris,
                Sepal.Area = Sepal.Length * Sepal.Width,
                Petal.Area = Petal.Length * Petal.Width)
    y <- scope(iris, dataset(Sepal.Area = Sepal.Length * Sepal.Width,
                             Petal.Area = Petal.Length * Petal.Width))
    expect_equal(x, y)
})


test_that("keys", {
    ds <- as.dataset(mtcars)
    keys(ds) <- keyset(name = rownames(mtcars))
    x <- select(ds, cyl, 2 * cyl, three = 3 * cyl)

    y <- dataset(cyl       = ds$cyl,
                 `2 * cyl` = 2 * ds$cyl,
                 three     = 3 * ds$cyl)
    keys(y) <- keys(ds)

    expect_equal(x, y)
})


test_that("wrong length", {
    expect_error(select(mtcars, cyl[1]),
                 "mismatch: data have 32 rows, selected values have 1")
})
