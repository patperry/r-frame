
context("cast")

test_that("to NULL", {
    expect_equal(cast(NULL, NULL), NULL)
    expect_equal(cast(NULL, integer()), NULL)
    expect_equal(cast(NULL, list()), NULL)
    expect_error(cast(NULL, 1),
                 "cannot cast from length-1 vector to NULL")
})


test_that("to logical", {
    expect_equal(cast(logical(), NULL), logical())
    expect_equal(cast(logical(), character()), logical())
    expect_equal(cast(logical(), c(0, 1, NA)), c(FALSE, TRUE, NA))
    expect_equal(cast(logical(), c("FALSE", "TRUE", NA)),
                 c(FALSE, TRUE, NA))
})


test_that("to raw", {
    expect_equal(cast(raw(), c(0, 1, 255)),
                 as.raw(c(0, 1, 255)))
})


test_that("to integer", {
    expect_equal(cast(integer(), c(0, 1, 255, -1, -3000, NA)),
                 as.integer(c(0, 1, 255, -1, -3000, NA)))
})


test_that("to double", {
    expect_equal(cast(double(), c("3.14", NA)), c(3.14, NA))
})


test_that("to complex", {
    expect_equal(cast(complex(), c("0+3.14i", NA)), c(0+3.14i, NA))
})


test_that("to character", {
    expect_equal(cast(character(), 1:5), as.character(1:5))
})


test_that("to factor", {
    type <- factor(letters)[0]
    expect_equal(cast(type, letters[4:1]),
                 structure(4:1, levels = letters, class = "factor"))
})


test_that("to factor with different levels", {
    f <- factor(c(), levels = letters)
    x1 <- cast(f, c("a", "e", "i", "o", "u"))
    expect_equal(x1, factor(c("a", "e", "i", "o", "u"), levels = letters))

    x2 <- cast(f, factor(c("a", "e", "i", "o", "u")))
    expect_equal(x1, factor(c("a", "e", "i", "o", "u"), levels = letters))

    expect_warning(cast(f, "A"), "invalid factor level, NA generated")
})


test_that("to Date from character", {
    type <- as.Date("2000-01-01")[0]
    expect_equal(cast(type, "1900-10-26"), as.Date("1900-10-26"))
})


test_that("to Date from POSIXct", {
    type <- as.Date("2000-01-01")[0]

    x1 <- as.POSIXct("2017-12-25 17:23:45", tz = "UTC")
    x2 <- as.POSIXct("2017-12-25 17:23:45", tz = "America/Los_Angeles")
    x3 <- as.POSIXct("2017-12-26 01:23:45", tz = "UTC")
    x4 <- as.POSIXct("2017-12-26 01:23:45", tz = "America/Los_Angeles")
    x <- list(x1, x2, x3, x4)
    y <- lapply(x, function(xi) cast(type, xi))

    z <- list(as.Date("2017-12-25"), as.Date("2017-12-25"),
              as.Date("2017-12-26"), as.Date("2017-12-26"))
    expect_equal(y, z)
})


test_that("to Date from POSIXlt", {
    type <- as.Date("2000-01-01")[0]
              
    x1 <- as.POSIXlt("2017-12-25 17:23:45", tz = "UTC")
    x2 <- as.POSIXlt("2017-12-25 17:23:45", tz = "America/Los_Angeles")
    x3 <- as.POSIXlt("2017-12-26 01:23:45", tz = "UTC")
    x4 <- as.POSIXlt("2017-12-26 01:23:45", tz = "America/Los_Angeles")
    x <- list(x1, x2, x3, x4)
    y <- lapply(x, function(xi) cast(type, xi))

    z <- list(as.Date("2017-12-25"), as.Date("2017-12-25"),
              as.Date("2017-12-26"), as.Date("2017-12-26"))
    expect_equal(y, z)
})


test_that("to POSIXct (UTC) from POSIXct or POSIXlt", {
    x1 <- as.POSIXct("2017-12-25 17:23:45", tz = "UTC")
    x2 <- as.POSIXct("2017-12-25 17:23:45", tz = "America/Los_Angeles")
    x3 <- as.POSIXct("2017-12-26 01:23:45", tz = "UTC")
    x4 <- as.POSIXct("2017-12-26 01:23:45", tz = "America/Los_Angeles")
    x <- list(x1, x2, x3, x4)

    y1 <- as.POSIXlt("2017-12-25 17:23:45", tz = "UTC")
    y2 <- as.POSIXlt("2017-12-25 17:23:45", tz = "America/Los_Angeles")
    y3 <- as.POSIXlt("2017-12-26 01:23:45", tz = "UTC")
    y4 <- as.POSIXlt("2017-12-26 01:23:45", tz = "America/Los_Angeles")
    y <- list(y1, y2, y3, y4)
              
    type <- x1[0]
    xx <- lapply(x, function(xi) cast(type, xi))
    yy <- lapply(y, function(yi) cast(type, yi))
    expect_equal(xx, yy)
})


test_that("to POSIXct (America/Los_Angeles) from POSIXct or POSIXlt", {
    x1 <- as.POSIXct("2017-12-25 17:23:45", tz = "UTC")
    x2 <- as.POSIXct("2017-12-25 17:23:45", tz = "America/Los_Angeles")
    x3 <- as.POSIXct("2017-12-26 01:23:45", tz = "UTC")
    x4 <- as.POSIXct("2017-12-26 01:23:45", tz = "America/Los_Angeles")
    x <- list(x1, x2, x3, x4)

    y1 <- as.POSIXlt("2017-12-25 17:23:45", tz = "UTC")
    y2 <- as.POSIXlt("2017-12-25 17:23:45", tz = "America/Los_Angeles")
    y3 <- as.POSIXlt("2017-12-26 01:23:45", tz = "UTC")
    y4 <- as.POSIXlt("2017-12-26 01:23:45", tz = "America/Los_Angeles")
    y <- list(y1, y2, y3, y4)
              
    type <- x2[0]
    xx <- lapply(x, function(xi) cast(type, xi))
    yy <- lapply(y, function(yi) cast(type, yi))
    expect_equal(xx, yy)
})


test_that("to record", {
    r <- record(int = integer(), lgl = logical(), chr = character())
    x <- record(a = 1.3, b = "TRUE", c = 3.14)
    y <- record(int = 1L, lgl = TRUE, chr = "3.14")
    expect_error(cast(r, x),
                 "mismatch: destination component 1 has name `int`, source has name `a`")

    expect_equal(cast(r, unname(x)), y)
    expect_equal(cast(unname(r), x), unname(y))

    names(x) <- names(r)
    expect_equal(cast(r, x), y)
})


test_that("to record with wrong components", {
    r <- record(int = integer(), lgl = logical(), chr = character())
    x <- record(int = 1L, lgl = TRUE, chr = "3.14", z = NULL)
    expect_error(cast(r, x), "mismatch: destination has 3 components, source has 4")
})


test_that("to list", {
    expect_equal(cast(list(), letters), as.list(letters))
})


test_that("to matrix", {
    t <- matrix(0L, 0, 3)
    x <- c(1.1, 2.4, 4.4)
    y <- as.record(list(1L, 2L, 4L))
    expect_equal(cast(t, x), y)
})

test_that("from matrix", {
    x <- matrix(1:6, 3, 2)
    t <- record(a = integer(), b = character())
    y <- cast(t, x)
    expect_equal(y, dataset(a = 1:3, b = as.character(4:6)))
})
