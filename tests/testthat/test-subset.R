context("subset")

test_that("on data.frame", {
    x <- as.dataset(mtcars)
    x <- subset(x, cyl == 4)
    y <- as.dataset(subset(mtcars, cyl == 4))
    expect_equal(x, y)
})


test_that("scoped", {
    x <- as.dataset(mtcars)
    gear <- 1:nrow(x)
    y1 <- subset(x, I(gear) == 5) # look for 'gear' in the environment
    y2 <- x[gear == 5, ]
    expect_equal(y1, y2)
})


test_that("wrong length", {
    expect_error(subset.dataset(mtcars, c(TRUE, FALSE)),
                 "mismatch: data has 32 rows, subset condition has 2")
})
