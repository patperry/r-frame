context("dataset-index")

test_that("[[<-", {
    x <- dataset(a = 1:5, b = letters[1:5], c = rep(TRUE, 5))
    x[[2]] <- 6:10
    y <- dataset(a = 1:5, b = 6:10, c = rep(TRUE, 5))
    expect_equal(x, y)
})
