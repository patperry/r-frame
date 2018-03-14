context("record entry")

test_that("numeric index", {
    x <- record(a = 1, b = 4, c = 19)

    expect_equal(x[[1]], 1)
    expect_equal(x[[2]], 4)
    expect_equal(x[[3]], 19)
    #expect_equal(x[[4]], NULL)
    expect_error(x[[4]], "bounds error: index is 4, maximum is 3")
})


test_that("NA index", {
    x <- record(a = 1, b = 4, c = 19)
    #expect_equal(x[[NA_character_]], NULL)
    expect_error(x[[NA_character_]], "unknown name <NA>")
})


test_that("missing index", {
    x <- record(a = 1, b = 4, c = 19)

    expect_error(x[[NULL]], "empty index")
    expect_error(x[[]], "empty index")
})


test_that("invalid index", {
    x <- record(a = 1, b = 4, c = 19)

    expect_error(x[[-1]] <- 1, "invalid index \\(-1\\)")
    expect_error(x[[0]] <- 1, "invalid index \\(0\\)")
    expect_error(x[[Inf]] <- 1, "invalid index \\(Inf\\)")
    expect_error(x[[TRUE]] <- 1, "invalid index type \\(logical\\)")
})


test_that("factor index", {
    x <- record(a = 1, b = 4, c = 19)
    i <- factor("b")
    expect_equal(x[[i]], 4)
})
