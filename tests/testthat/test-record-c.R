context("record c")

test_that("empty", {
    expect_true(is.record(c.record()))
})


test_that("list", {
    l <- list(a = 1, b = 2)
    x <- as.record(l)
    expect_equal(c.record(l), x)
})


test_that("empty list", {
    l <- list()
    x <- as.record(l)
    expect_equal(c.record(l), x)
})


test_that("named empty list", {
    l <- structure(list(), names = character())
    x <- as.record(l)
    expect_equal(c.record(l), x)
})


test_that("two lists", {
    l1 <- list(a = 1, b = 2)
    l2 <- list(c = 3)
    x1 <- as.record(l1)
    x2 <- as.record(l2)
    x <- as.record(c(l1, l2))

    expect_equal(c.record(l1, l2), x)
})


test_that("nested single argument", {
    l <- list(a = 1, b = record(x = "foo", y = "bar"))
    expect_equal(c.record(l), as.record(l))
})


test_that("mixed types", {
    x <- c.record(list(x = "foo"), NULL, "bar")
    y <- as.record(list(x = "foo", "bar"))
    expect_equal(x, y)
})
