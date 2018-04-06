context("do")

test_that("unnamed result", {
    x <- dataset(n = 1:10)
    x <- do(x, function(n) 10 * n)
    y <- as.dataset.record(list(10 * (1:10)))
    expect_equal(x, y)
})


test_that("named result", {
    x <- dataset(n = 1:10)
    x <- do(x, function(n) c(a = 10 * n))
    y <- dataset(a = 10 * (1:10))
    expect_equal(x, y)
})


test_that("multiple results", {
    x <- dataset(n = 1:10)
    x <- do(x, function(n)
            record(a =  10 * n,
                   b = -10 * n,
                   c = "foo"))
    y <- dataset(a =  10 * (1:10),
                 b = -10 * (1:10),
                 c = rep("foo", 10))
    expect_equal(x, y)
})


test_that("multiple inputs", {
    ds <- dataset(count = c("one", "two", "three"),
                  fruit = c("banana", "banana", "orange"))
    x <- do(ds, function(c, f) c(fun = paste(c, f)))
    y <- dataset(fun = paste(ds$count, ds$fruit))
    expect_equal(x, y)
})


test_that("empty input", {
    x <- dataset(n = integer())
    x <- do(x, function(n) 10 * n)
    y <- NULL
    expect_equal(x, y)
})


test_that("data.frame input", {
    df <- mtcars[, "cyl"]
    x <- do(df, function(c) 2 * c)
    y <- do.dataset(df, function(c) 2 * c)
    expect_equal(x, y)
})
