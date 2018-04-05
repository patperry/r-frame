
context("unique")

test_that("logical", {
    x <- dataset(col = c(TRUE, FALSE, NA, FALSE, TRUE, NA))
    expect_equal(anyDuplicated(x), anyDuplicated(x$col))
    expect_equal(duplicated(x), duplicated(x$col))
    expect_equal(unique(x), dataset(col = unique(x$col)))
})


test_that("raw", {
    x <- dataset(col = as.raw(c(0, 1, 1, 3, 1, 255)))
    expect_equal(anyDuplicated(x), anyDuplicated(x$col))
    expect_equal(duplicated(x), duplicated(x$col))
    expect_equal(unique(x), dataset(col = unique(x$col)))
})


test_that("integer", {
    x <- dataset(col = as.integer(c(-99, NA, -99, 8, 8, 0, 1, 1, 3, 1, 0,
                                    77, 255, NA, 12, 13, 12, -1, NA, -1)))
    expect_equal(anyDuplicated(x), anyDuplicated(x$col))
    expect_equal(duplicated(x), duplicated(x$col))
    expect_equal(unique(x), dataset(col = unique(x$col)))
})


test_that("double", {
    x <- dataset(col = c(0.42, NA, -0.06, NaN, 0.42, NA, 0.42, NA,
                         NA, -0.06, 0.42, 0.27, 0.81, Inf, Inf, 0.27, 0.42,
                         0.27, 0.27, 0.13, -0.06, -0.06, NaN, -Inf,
                         0.15, 0.1 + 0.05))
    expect_equal(anyDuplicated(x), anyDuplicated(x$col))
    expect_equal(duplicated(x), duplicated(x$col))
    expect_equal(unique(x), dataset(col = unique(x$col)))
})


test_that("complex", {
    set.seed(0)
    x <- dataset(col = (sample(c(NA, rnorm(5)), 50, replace = TRUE)
                        + 1i * sample(c(NA, rnorm(4)), 50, replace = TRUE)))
    expect_equal(anyDuplicated(x), anyDuplicated(x$col))
    expect_equal(duplicated(x), duplicated(x$col))
    expect_equal(unique(x), dataset(col = unique(x$col)))
})


test_that("character", {
    set.seed(0)

    set <- c(letters)
    for (i in 1:4) {
        set <- c(set, paste0(set, set))
    }

    x <- dataset(col = sample(c("", NA, set), 1000, replace = TRUE))
    expect_equal(anyDuplicated(x), anyDuplicated(x$col))
    expect_equal(duplicated(x), duplicated(x$col))
    expect_equal(unique(x), dataset(col = unique(x$col)))
})


test_that("character with empty, NA", {
    set.seed(0)
    set <- c(NA, NA, "", "", "a", "b", "c")
    x <- dataset(col = sample(set, 100, replace = TRUE))
    expect_equal(anyDuplicated(x), anyDuplicated(x$col))
    expect_equal(duplicated(x), duplicated(x$col))
    expect_equal(unique(x), dataset(col = unique(x$col)))
})


test_that("large set", {
    set.seed(0)
    x <- dataset(col = sample.int(200, 1000, replace = TRUE))
    expect_equal(anyDuplicated(x), anyDuplicated(x$col))
    expect_equal(duplicated(x), duplicated(x$col))
    expect_equal(unique(x), dataset(col = unique(x$col)))
})


test_that("mixed encoding", {
    col <- c("fa\u00E7ile", "fa\xE7ile")
    Encoding(col[2]) <- "latin1"
    x <- dataset(col = col)
    expect_equal(anyDuplicated(x), anyDuplicated(x$col))
    expect_equal(duplicated(x), duplicated(x$col))
    expect_equal(unique(x), dataset(col = unique(x$col)))
})


test_that("fixes bugs in base R", {
    x <- as.dataset(data.frame(x = c(.15, .1 + .05), y = "a"))
    expect_equal(unique(x), x)
})


test_that("works for empty dataset", {
    x <- as.dataset(structure(list(), row.names = .set_row_names(3),
                              class = "data.frame"))
    expect_equal(unique(x), x[1, ])
})


test_that("converts to dataset", {
    expect_true(is.dataset(unique.dataset(mtcars)))
})


test_that("works for keyset", {
    k <- keyset(x = 1:10)
    expect_equal(anyDuplicated(k), 0L)
    expect_equal(duplicated(k), logical(10))
    expect_equal(unique(k), k)
})
