
context("dataset")


test_that("'dataset' allows NULL names", {
    x <- as.dataset(as.record(list(1, 2, 3)))
    expect_equal(names(x), NULL)
})


test_that("'as.dataset' allows duplicate column names", {
    expect_equal(dataset(x = 1:10, y = 1:10, x = 1:10),
                 as.dataset(record(x = 1:10, y = 1:10, x = 1:10)))
})


test_that("'as.dataset' allows duplicate names after normalization", {
    x <- record(1, 2)
    names(x) <- c("\u00c5", "\u212b")
    expect_equal(names(as.dataset(x)), names(x))
})


test_that("'as.dataset' allows empty names", {
    x <- structure(record(4, 7, 2, -9, 1),
                   names = c("a", "b", "", "d", ""))
    expect_equal(names(as.dataset(x)), names(x))
})


test_that("'as.dataset.list' works for missing names", {
    x <- as.dataset(as.record(list(a = 1, -5, 2.3)))
    expect_equal(names(x), c("a", "", ""))
})


test_that("'as.dataset.list' allows duplicated names", {
    r <- record(a = 1, a = -5, a = 2.3)
    x <- as.dataset(r)
    expect_equal(names(x), names(r))
})


test_that("'dataset' errors for unequal length columns", {
    expect_error(dataset(x = 1:10, y = 1:10, z = c(1, 2, 3)),
                 "mismatch: column 3 (`z`) has 3 rows, should have 10",
                 fixed = TRUE)

    expect_error(dataset(x = 1:10, y = matrix(1:10, 2, 5)),
                 "mismatch: column 2 (`y`) has 2 rows, should have 10",
                 fixed = TRUE)
})


test_that("'dataset' errors for rank-3 array columns", {
    expect_error(dataset(x = array(1:24, c(2,3,4))),
                 "column 1 (`x`) has more than 2 dimensions",
                 fixed = TRUE)
})


test_that("'dataset' does not allow scalar columns", {
    expect_error(dataset(x = 1:10, y = 10:1, z = 1),
                 "mismatch: column 3 (`z`) has 1 rows, should have 10",
                 fixed = TRUE)
})


test_that("'dataset' keeps vector names", {
    x <- 1:26
    names(x) <- letters
    d <- dataset(x)
    expect_equal(d[[1]], x)
})


test_that("'dataset' keeps 1-d array names", {
    x <- array(1:26, dimnames = list(letters))
    d <- dataset(x)
    expect_equal(d[[1]], x)
})


test_that("'dataset' keeps matrix row names", {
    x <- matrix(1:20, 4, 5, dimnames = list(letters[1:4], LETTERS[1:5]))
    d <- dataset(x)
    expect_equal(d[[1]], x)
})


test_that("'as.dataset' works for nested", {
    r <- record(zz = record(a = 1:4))
    x <- as.dataset(r)
    expect_equal(x, dataset(zz = dataset(a = 1:4)))
})


test_that("'as.dataset' works with no columns", {
    x <- mtcars
    x[] <- NULL
    x <- as.dataset(x)

    expect_equal(nrow(x), nrow(mtcars))
    expect_equal(ncol(x), 0)
    expect_equal(keys(x), keys(as.dataset(mtcars)))
})


test_that("'as.dataset' ignores data.frame row names", {
    x1 <- data.frame(x = 1:26, row.names = letters)
    x2 <- dataset(x = 1:26)
    expect_equal(as.dataset(x1), x2)
})


test_that("'as.dataset' does not require names", {
    expect_equal(names(as.dataset(list("hello"))), NULL)
})


test_that("'dataset' can be nested", {
    x <- dataset(a = letters[1:5], b = rnorm(5))
    expect_equal(dim(x), c(5, 2))
    expect_equal(names(x), c("a", "b"))

    y <- dataset(a = c(3,2,7,8,-1), b = x, c = rnorm(5))
    expect_equal(dim(y), c(5, 3))
    expect_equal(names(y), c("a", "b", "c"))

    z <- dataset(a = y, b = LETTERS[6:10])
    expect_equal(dim(z), c(5, 2))
    expect_equal(names(z), c("a", "b"))
})


test_that("as.dataset(record()) has one row", {
    x <- as.dataset(record())
    expect_equal(dim(x), c(1, 0))
})


test_that("empty column", {
    x <- dataset(a = 1:5, b = 1:5, c = record())
    expect_equal(dim(x), c(5, 3))
    expect_equal(dim(x$c), c(5, 0))
})


test_that("nested empty column", {
    x <- dataset(c = record(foo = record()), b = 1:6)
    expect_equal(dim(x), c(6, 2))
    expect_equal(dim(x$c), c(6, 1))
    expect_equal(dim(x$c$foo), c(6, 0))
})


test_that("handles list matrix", {
    mat <- rbind(list("D", 3),
                 list("A", 1),
                 list("C", 1))
    x <- as.dataset(mat)
    y <- as.dataset(as.record(list(mat[, 1], mat[, 2])))
    expect_equal(x, y)
})


test_that("errors for list array", {
    arr <- array(as.list(1:6), c(1, 2, 3))
    expect_error(as.dataset(arr), "cannot convert rank-3 array to dataset")
})


test_that("from NULL", {
    expect_equal(dim(as.dataset(NULL)), c(0, 0))
})


test_that("from matrix", {
    mat <- matrix(1:20, 4, 5)
    colnames(mat) <- letters[1:5]
    x <- as.dataset(mat)
    y <- dataset(a = 1:4, b = 5:8, c = 9:12, d = 13:16, e = 17:20)
    expect_equal(x, y)

    x2 <- as.dataset.default(mat)
    expect_equal(x, x2)
})
