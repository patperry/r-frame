context("transform")

test_that("modify existing", {
    x <- transform.dataset(mtcars, cyl = 2 * cyl)
    y <- as.dataset(transform(mtcars, cyl = 2 * cyl))
    expect_equal(x, y)
})


test_that("add new", {
    x <- transform.dataset(iris, area = Sepal.Length * Sepal.Width)
    y <- as.dataset(transform(iris, area = Sepal.Length * Sepal.Width))
    expect_equal(x, y)
})
