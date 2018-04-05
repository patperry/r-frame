context("keys")

test_that("data.frame", {
    expect_equal(keys(mtcars), NULL)
})


test_that("set and reset", {
    x <- as.dataset(mtcars)

    k1 <- keyset(name = rownames(mtcars))
    keys(x) <- k1
    expect_equal(keys(x), k1)

    k2 <- keyset(id = seq_len(nrow(mtcars)))
    keys(x) <- k2
    expect_equal(keys(x), k2)

    keys(x) <- NULL
    expect_equal(keys(x), NULL)
})


test_that("wrong number of rows", {
    x <- as.dataset(mtcars)
              
    expect_error(keys(x) <- 1,
                 "mismatch: data have 32 rows, keys have 1")
})


test_that("keys on keyset", {
    x <- keyset(foo = 1:5)
    y <- keyset(foo = 1:5)
    keys(x) <- keyset(key = LETTERS[1:5])
    keys(y) <- keyset(key = LETTERS[1:5])
    expect_equal(x, y)
})
