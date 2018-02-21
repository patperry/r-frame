context("record-index")

test_that("rename fields", {
    x <- record(a = 7, b = 12, c = "hello")
    expect_equal(x[c(z = 1, y = 2)], record(z = 7, y = 12))
})

test_that("rank-3 array", {
    x <- record(a = 7, b = 12, c = "hello")
    expect_error(x[array(1, c(1, 1, 1))],
                 "cannot index with rank-3 array")
})

test_that("numeric object", {
    x <- c.record(1:10, a = "aaa")
    i <- as.hexmode(10)
    expect_equal(x[[i]], x[[10]])
    expect_equal(x[i], x[10])
})

test_that("numeric NA", {
    x <- record(1)
    expect_error(x[NA_real_], "numeric index cannot contain NA values")
})


test_that("mixed sign", {
    x <- record(1)
    expect_error(x[c(0, -1)],
                 "numeric index cannot contain both negative and non-negative values")
    expect_error(x[c(-1, 0)],
                 "numeric index cannot contain both negative and non-negative values")
})


test_that("out-of-bounds", {
    x <- record(1)
    expect_equal(x[2], as.record(structure(list(NULL), names = "")))
})


test_that("wrong length logical mask", {
    x <- record(1, 2, 3, 4)
    i <- c(TRUE, FALSE)
    expect_error(x[i], "mismatch: logical mask length is 2, object length is 4")
})


test_that("unknown name", {
    x <- record(aa = 1)
    expect_equal(x["a"], record(a = NULL))
})


test_that("factor index", {
    x <- record(a = 5, b = 4, c = 3)
    i <- factor("b")
    expect_equal(x[i], record(b = 4))
})


test_that("$", {
    x <- record(a = 1, b = 13, cc = 10)
    expect_equal(x$b, 13)
})


test_that("$<- existing", {
    x <- record(a = 1, b = 13, cc = 10)
    x$b <- "foo"
    expect_equal(x, record(a = 1, b = "foo", cc = 10))
})


test_that("$<- new", {
    x <- record(a = 1, b = 13, cc = 10)
    x$c <- "foo"
    expect_equal(x, record(a = 1, b = 13, cc = 10, c = "foo"))
})


test_that("NULL, missing index", {
    x <- record(a = 1, b = 17)
    expect_equal(x[NULL], x)
    expect_equal(x[], x[NULL])
})


test_that("negative idnex", {
    x <- record(a = 1, b = 17, c = 2, d = 19)
    expect_equal(x[c(-9, -2, -3, -2)], x[c(1, 4)])
})


test_that("logical index", {
    x <- record(a = 1, b = 17, c = 2, d = 19)
    expect_equal(x[c(TRUE, TRUE, FALSE, FALSE)], x[c(1, 2)])
})


test_that("logical object", {
    x <- record(a = 1, b = 17, c = 2, d = 19)
    i <- structure(c(foo = TRUE, FALSE, bar = TRUE, FALSE), class = "foo")
    expect_equal(x[i], x[c(foo = 1, bar = 3)])
})


test_that("nested assign", {
    x <- record(a = 1, b = list("foo", "bar", "baz"), c = 18, d = -9)
    x[[c(2, 3)]] <- "!!"
    y <- record(a = 1, b = list("foo", "bar", "!!"), c = 18, d = -9)
    expect_equal(x, y)
})


test_that("list assign", {
    x <- record(a = 1, b = 2)
    x[[1]] <- list(1, 2, 3)
    expect_equal(x, record(a = list(1, 2, 3), b = 2))
})


test_that("field delete", {
    x <- record(a = 1, b = 2, c = 3)
    x[[2]] <- NULL
    expect_equal(x, record(a = 1, c = 3))
})


test_that("nested delete", {
    x <- record(a = 1, b = list("foo", "bar", "baz"), c = 18, d = -9)
    x[[c(2, 3)]] <- NULL
    y <- record(a = 1, b = list("foo", "bar"), c = 18, d = -9)
    expect_equal(x, y)
})


test_that("delete all", {
    x <- record(a = 1, b = 77, c = "foo")
    y <- x
    x[NULL] <- NULL
    y[] <- NULL
    expect_equal(x, record())
    expect_equal(y, x)
})


test_that("delete by name", {
    x <- record(a = 1, b = 77, c = "foo")
    x[c("a", "b")] <- NULL
    expect_equal(x, record(c = "foo"))
})


test_that("replace wrong number", {
    x <- record(a = 1, b = 2, c = 3, d = 4)
    expect_error(x[NULL] <- c(100, 101),
                 "mismatch: selection length is 4, replacement length is 2")
})


test_that("replace with 0", {
    x <- record(a = 1, b = 2, c = 3, d = 4)
    x[c(0, 0, 1)] <- c(100, 200, 300)
    expect_equal(x, record(a = 300, b = 2, c = 3, d = 4))
})
