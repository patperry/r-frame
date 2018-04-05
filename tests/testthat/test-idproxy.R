context("idproxy")

test_that("atomic", {
    expect_equal(idproxy(letters), letters)
    expect_equal(idproxy(NULL), NULL)
    expect_equal(idproxy(1:10), 1:10)
})


test_that("vector object", {
    expect_equal(idproxy(as.hexmode(10:1)),
                 xtfrm(as.hexmode(10:1)))
})


test_that("invalid", {
    expect_error(idproxy(sin),
                 'cannot compute idproxy for objects of class "function"')
})


test_that("matrix", {
    x <- matrix(1:20, 4, 5)
    y <- as.dataset.record(list(1:4, 5:8, 9:12, 13:16, 17:20))
    expect_equal(idproxy(x), y)
    expect_equal(idproxy.default(x), y)
})


test_that("array", {
    x <- array(1, c(1, 1, 1))
    expect_error(idproxy(x), "cannot compute idproxy for rank-3 objects")
})
