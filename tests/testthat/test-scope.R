context("scope")

test_that("get column", {
    x <- scope(mtcars, cyl)
    y <- mtcars$cyl
    expect_equal(x, y)
})


test_that("get constant", {
    expect_equal(scope(mtcars, c(99, 100)), c(99, 100))
})


test_that("get env var", {
    expect_equal(scope(mtcars, letters), letters)
})


test_that("get env var with I()", {
    expect_equal(scope(mtcars, I(letters)), letters)
})


test_that("shadow column", {
    cyl <- letters
    x <- scope(mtcars, I(cyl))
    y <- cyl
    expect_equal(x, y)
})


test_that("I(I())", {
    expect_equal(scope(mtcars, I(I(letters))), I(letters))
})


test_that("f(I())", {
    cyl <- 1:10
    x <- scope(mtcars, sin(I(cyl)))
    y <- sin(cyl)
    expect_equal(x, y)
})


test_that("I(f())", {
    cyl <- 1:10
    x <- scope(mtcars, I(sin(cyl)))
    y <- sin(cyl)
    expect_equal(x, y)
})


test_that("f(f())", {
    cyl <- 1:10
    x <- scope(mtcars, sin(sin(cyl)))
    y <- sin(sin(mtcars$cyl))
    expect_equal(x, y)
})


test_that("f(I(f()))", {
    cyl <- 1:10
    x <- scope(mtcars, sin(I(sin(cyl))))
    y <- sin(sin(cyl))
    expect_equal(x, y)
})


test_that("unknown variable", {
    expect_error(scope(mtcars, zzz), "object 'zzz' not found")
})


test_that("wrong number of args", {
    expect_error(scope(mtcars, I()), "empty I() call", fixed = TRUE)
    expect_error(scope(mtcars, I(1, 2)), "too many arguments (2) to I()",
                 fixed = TRUE)
})
