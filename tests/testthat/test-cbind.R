context("cbind")

test_that("'cbind' concatenates columns", {
    x1 <- mtcars[, 1:3]
    x2 <- mtcars[, 4, drop = FALSE]
    x3 <- mtcars[, 5:8]
    y <- cbind.dataset(x1, x2, x3)
    expect_equal(y, as.dataset(mtcars[, 1:8]))
})


test_that("'cbind' NULL works", {
    expect_equal(cbind.dataset(), NULL)
    expect_equal(cbind.dataset(NULL), NULL)
    expect_equal(cbind.dataset(NULL, NULL), NULL)
})


test_that("'cbind' ignores NULL", {
    x1 <- mtcars[,1:3]
    x2 <- mtcars[,4:8]
    y <- cbind.dataset(x1, NULL, NULL, x2, NULL)
    expect_equal(y, cbind.dataset(x1, x2))
})


test_that("'cbind' errors if rows do not match", {
    x1 <- mtcars[,1:3]
    x2 <- mtcars[1:4, 4:5]
    expect_error(cbind.dataset(x1, x1, x2),
                 "mismatch: argument 1 has 32 rows, argument 3 has 4")
})


test_that("'cbind' errors if rows do not match, with NULL in between", {
    x1 <- mtcars[,1:3]
    x2 <- mtcars[1:4, 4:5]
    expect_error(cbind.dataset(NULL, x1, x1, NULL, x2),
                 "mismatch: argument 2 has 32 rows, argument 5 has 4")
})


test_that("'cbind' errors if keys do not match", {
    x1 <- mtcars[,1:3]
    x2 <- mtcars[rev(seq_len(nrow(x1))),4:5]
    expect_error(cbind.dataset(x1, x2),
                 "arguments 1 and 2 have different keys")
})


test_that("'cbind' allows NULL keys on second", {
    x1 <- mtcars[,1:3]
    x2 <- mtcars[,4:5]
    d2 <- as.dataset(x2)
    keys(d2) <- NULL
    y <- cbind.dataset(x1, d2)
    expect_equal(y, cbind.dataset(x1, x2))
})


test_that("'cbind' allows NULL keys on first", {
    x1 <- mtcars[,1:3]
    x2 <- mtcars[,4:5]
    d1 <- as.dataset(x1)
    keys(d1) <- NULL
    y <- cbind.dataset(d1, x2)
    expect_equal(y, cbind.dataset(x1, x2))
})


test_that("'cbind' allows NULL keys on all", {
    x1 <- mtcars[,1:3]
    x2 <- mtcars[,4:5]
    d1 <- as.dataset(x1); keys(d1) <- NULL
    d2 <- as.dataset(x2); keys(d2) <- NULL
    y <- cbind.dataset(d1, d2)

    d3 <- cbind.dataset(x1, x2)
    keys(d3) <- NULL
    expect_equal(y, d3)
})


test_that("'cbind' can handle named vector arguments", {
    x <- cbind.dataset(name = rownames(mtcars), mtcars)
    y <- cbind.dataset(dataset(name = rownames(mtcars)), mtcars)
    expect_equal(x, y)
})


test_that("'cbind' can handle unnamed vector arguments", {
    vec <- rownames(mtcars)
    x <- cbind.dataset(vec, mtcars)
    y <- cbind.dataset(dataset(vec = rownames(mtcars)), mtcars)
    expect_equal(x, y)
})


test_that("'cbind' can handle named matrix arguments", {
    x <- cbind.dataset(name = rownames(mtcars), nest = mtcars)
    nest_mtcars <- `names<-`(mtcars,  paste0("nest.", names(mtcars)))
    y <- cbind.dataset(dataset(name = rownames(mtcars)), nest_mtcars)
    expect_equal(x, y)
})


test_that("'cbind' can handle named matrix arguments 2", {
    mtcars <- unname(mtcars)
    x <- cbind.dataset(name = rownames(mtcars), nest = mtcars)
    nest_mtcars <- `names<-`(mtcars,  paste0("nest.", seq_along(mtcars)))
    y <- cbind.dataset(dataset(name = rownames(mtcars)), nest_mtcars)
    expect_equal(x, y)
})


test_that("'cbind' can handle named matrix arguments with one column", {
    mtcars1 <- mtcars[, 1, drop = FALSE]
    x <- cbind.dataset(name = rownames(mtcars), nest = mtcars1)
    nest_mtcars1 <- `names<-`(mtcars1,  paste0("nest.", names(mtcars1)))
    y <- cbind.dataset(dataset(name = rownames(mtcars)), nest_mtcars1)
    expect_equal(x, y)
})


test_that("'cbind' can handle named matrix arguments with one column 2", {
    mtcars1 <- mtcars[, 1, drop = FALSE]
    names(mtcars1) <- NULL
    x <- cbind.dataset(name = rownames(mtcars), nest = mtcars1)
    nest_mtcars1 <- `names<-`(mtcars1,  paste0("nest.", seq_along(mtcars1)))
    y <- cbind.dataset(dataset(name = rownames(mtcars)), nest_mtcars1)
    expect_equal(x, y)
})


test_that("'cbind' can handle named with 0 columns", {
    z <- mtcars[,FALSE]
    x <- cbind.dataset(mtcars, foo = z)
    y <- cbind.dataset(mtcars, z)
    expect_equal(x, y)
})


test_that("'cbind' with 0 columns works", {
    x <- as.dataset(mtcars)[, 0]
    expect_equal(cbind.dataset(x, x, x), x)
})
