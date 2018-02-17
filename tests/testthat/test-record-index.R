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
    expect_error(x[2], "numeric index exceeds object length")
})


test_that("wrong length logical mask", {
    x <- record(1, 2, 3, 4)
    i <- c(TRUE, FALSE)
    expect_error(x[i], "mismatch: logical mask length is 2, object length is 4")
})


test_that("unknown name", {
    x <- record(aa = 1)
    expect_error(x["a"], "index contains unknown name \"a\"")
})


test_that("factor index", {
    x <- record(a = 5, b = 4, c = 3)
    i <- factor("b")
    expect_equal(x[i], record(b = 4))
})
