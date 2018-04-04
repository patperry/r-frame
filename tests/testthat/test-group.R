context("group")


test_that("'group(,integer)' splits", {
    set.seed(0)
    group <- sample(1:5, nrow(mtcars), replace = TRUE)
    x <- group(mtcars, group)
    l <- lapply(split(mtcars, group), as.dataset)
    l0 <- l; names(l0) <- NULL
    y <- as.dataset(record(group = l0))
    keys(y) <-  keyset(group = as.integer(names(l)))
    expect_equal(reorder(x, keys(x)), y)
})


test_that("'group' + 'do' works with scalar function on parts", {
    set.seed(0)
    g <- sample(1:5, nrow(mtcars), replace = TRUE)
    xg <- group(mtcars, g = I(g))
    x <- as.dataset(sapply(xg[[1]], nrow))
    keys(x) <-  keys(xg)

    y <- group(mtcars, dataset(g))
    y <- do(y, nrow)
    expect_equal(x, y)
})


test_that("'group' + 'do' works with NA", {
    x <- dataset(
        tailnum = c(NA, "N763JB", "N329JB", "N618JB", "N172US", "N78511"),
        arr_time = c(NA, 504L, 203L, 700L, 650L, 830L))
    y <- group(x, tailnum)
    y <- do(y, nrow)

    keys <- unique(x[,"tailnum"])
    z <- as.dataset.record(list(rep(1L, nrow(keys))))
    keys(z) <- keys
    expect_equal(y, z)
})
