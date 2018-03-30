context("rbind")

test_that("'rbind' concatenates rows", {
    x1 <- mtcars[1:3, ]
    x2 <- mtcars[4, , drop = FALSE]
    x3 <- mtcars[5:8, ]
    y <- rbind.dataset(x1, x2, x3)
    expect_equal(y, as.dataset(mtcars[1:8, ]))
})


test_that("'rbind' NULL works", {
    expect_equal(rbind.dataset(), NULL)
    expect_equal(rbind.dataset(NULL), NULL)
    expect_equal(rbind.dataset(NULL, NULL), NULL)
})


test_that("'rbind' ignores NULL", {
    x1 <- mtcars[1:3, ]
    x2 <- mtcars[4:8, ]
    y <- rbind.dataset(x1, NULL, NULL, x2, NULL)
    expect_equal(y, rbind.dataset(x1, x2))
})


test_that("'rbind' errors if columns do not match", {
    x1 <- mtcars[1:3, ]
    x2 <- mtcars[4:5, 1:4]
    expect_error(rbind.dataset(x1, x1, x2),
                 "arguments 1 and 3 have different numbers of columns")
})


test_that("'rbind' errors if columns do not match, with NULL in between", {
    x1 <- mtcars[1:3, ]
    x2 <- mtcars[4:5, 1:4]
    expect_error(rbind.dataset(NULL, x1, x1, NULL, x2),
                 "arguments 2 and 5 have different numbers of columns")
})


test_that("'rbind' errors if names do not match", {
    x1 <- mtcars[1:3, ]
    x2 <- mtcars[4:5, rev(seq_len(ncol(x1)))]
    expect_error(rbind.dataset(x1, x2),
                 "arguments 1 and 2 have different names")
})


test_that("'rbind' allows NULL names on second", {
    x1 <- mtcars[1:3, ]
    x2 <- mtcars[4:5, ]
    y <- rbind.dataset(x1, unname(x2))
    expect_equal(y, rbind.dataset(x1, x2))
})


test_that("'rbind' allows NULL names on first", {
    x1 <- mtcars[1:3, ]
    x2 <- mtcars[4:5, ]
    y <- rbind.dataset(unname(x1), x2)
    expect_equal(y, rbind.dataset(x1, x2))
})


test_that("'rbind' allows NULL names on all", {
    x1 <- mtcars[1:3, ]
    x2 <- mtcars[4:5, ]
    y <- rbind.dataset(unname(x1), unname(x2))
    expect_equal(y, unname(rbind.dataset(x1, x2)))
})


test_that("'rbind' errors for mismatched key names", {
    x1 <- as.dataset(mtcars[1:3, ])
    x2 <- as.dataset(mtcars[4:5, ])
    keys(x2) <- dataset(foo = keys(x2)[[1]])
    expect_error(rbind.dataset(x1, x2),
                 "arguments 1 and 2 have different key names")
})


test_that("'rbind' errors for mismatched number of keys", {
    x1 <- as.dataset(mtcars[1:3, ])
    x2 <- as.dataset(mtcars[4:5, ])
    keys(x2) <- dataset(foo = keys(x2)[[1]], bar = 4:5)
    expect_error(rbind.dataset(x1, x2),
                 "arguments 1 and 2 have different numbers of keys")
})


test_that("'rbind' can handle named vector arguments", {
    x <- rbind.dataset(first = mtcars[1, , drop = TRUE], mtcars)
    y <- as.dataset(rbind(first = mtcars[1, , drop = TRUE], mtcars))
    expect_equal(x, y)
})


test_that("'rbind' can handle named vector arguments with unnamed matrix", {
    x <- rbind.dataset(first = mtcars[1, , drop = TRUE], unname(mtcars))
    y <- as.dataset(rbind(first = mtcars[1, , drop = TRUE], mtcars))
    expect_equal(x, y)
})


test_that("'rbind' can handle unnamed vector arguments", {
    z <- mtcars
    rownames(z) <- NULL
    x <- rbind.dataset(mtcars[1, , drop = TRUE], z)
    y <- rbind(mtcars[1, , drop = TRUE], z)
    expect_equal(x, as.dataset(y))
})


test_that("'rbind' can handle unnamed vector arguments with unnamed matrix", {
    z <- mtcars
    rownames(z) <- NULL
    x <- rbind.dataset(mtcars[1, , drop = TRUE], unname(z))
    y <- rbind(mtcars[1, , drop = TRUE], z)
    expect_equal(x, as.dataset(y))
})


test_that("'rbind' can handle named matrix arguments", {
    z <- mtcars
    rownames(z) <- NULL
    x <- rbind.dataset(mtcars[1, , drop = FALSE], nest = z)
    y <- as.dataset(rbind(mtcars[1, , drop = FALSE], nest = z))
    expect_equal(x, y)
})


test_that("'rbind' can handle duplicate keys", {
    x1 <- as.dataset(mtcars[1:6, ])
    x2 <- as.dataset(mtcars[1:5, ])
    x <- rbind.dataset(x1, x2)

    y1 <- `keys<-`(x1, cbind(keys(x1), "#" = rep(1L, 6)))
    y2 <- `keys<-`(x2, cbind(keys(x2), "#" = rep(2L, 5)))
    y <- rbind.dataset(y1, y2)

    expect_equal(x, y)
})


test_that("'rbind' works with non-NULL, 0 keys", {
    k <- as.keyset(structure(list(), row.names = .set_row_names(1),
                             class = "data.frame"))
    x1 <- as.dataset(list(11)); keys(x1) <- k
    x2 <- as.dataset(list(22)); keys(x2) <- k
    x <- rbind.dataset(x1, x2)

    y1 <- x1; keys(y1) <- keyset("#" = 1L)
    y2 <- x2; keys(y2) <- keyset("#" = 2L)
    y <- rbind.dataset(y1, y2)

    expect_equal(x, y)
})


test_that("'rbind' works with matrix columns", {
    a1 <- matrix(1:6, 2, 3)
    a2 <- matrix(7:9, 1, 3)
    x <- rbind(dataset(a = a1), dataset(a = a2))
    y <- dataset(a = rbind(a1, a2))
    expect_equal(x, y)
})
