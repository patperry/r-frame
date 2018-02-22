context("record")

test_that("without names", {
    xx <- "foo"
    expect_equal(record(2, xx, -8),
                 as.record(list("2" = 2, "xx" = "foo", "-8" = -8)))
})


test_that("with names", {
    expect_equal(record(a = 3, b = "hello"),
                 as.record(list(a = 3, b = "hello")))
})


test_that("set wrong names", {
    x <- record(a = 1, b = 2)
    expect_error(names(x) <- "foo",
                 "mismatch: `value` length is 1, object length is 2")
})


test_that("as.record wrong names", {
    x <- record(a = 1, b = 2)
    expect_error(as.record(x, names = c("a", "bb")),
                 "argument names do not match")
})

test_that("c", {
    x <- record(a = 1, b = 2)
    y <- record(c = "foo", d = "bar")
    expect_equal(c.record(x, y), record(a = 1, b = 2, c = "foo", d = "bar"))
})


test_that("c with prefix", {
    x <- record(a = 1)
    y <- record(b = 2)
    expect_equal(c.record(x, y = y), record(a = 1, y.b = 2))
})


test_that("c with no suffix", {
    expect_equal(c.record(x = as.record(list(1)), y = as.record(list("b", "d"))),
                 record(x.1 = 1, y.1 = "b", y.2 = "d"))
})


test_that("c with NULL", {
    x <- record(a = 1, b = 2)
    expect_equal(c.record(NULL, x, NULL, NULL, x),
                 c.record(x, x))
})


test_that("set length", {
    x <- record(a = 1, b = "foo")
    length(x) <- 1
    expect_equal(x, record(a = 1))
})
