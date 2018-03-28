context("lookup")

test_that("empty", {
    expect_equal(lookup(1:10, keyset(integer())),
                 rep_len(NA_real_, 10))
})


test_that("small integer", {
    k <- keyset(1:10)
    for (i in 1:10) {
        expect_equal(lookup(i, k), i)
    }
})


test_that("big integer", {
    set.seed(0)
    values <- sample.int(1000, 200)
    k <- keyset(key = values)
    expect_equal(lookup(values, k), seq_along(values))
})
