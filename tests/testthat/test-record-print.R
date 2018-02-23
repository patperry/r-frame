context("record-print")

test_that("empty", {
    x <- record()
    expect_equal(capture_output(print(x)), "(0 entries)")
})

test_that("missing names", {
    x <- record(a = 1, b = 2, c = "foo")
    names(x)[[2]] <- ""

    lines <- c(
"a     : 1",
"[[2]] : 2",
"c     : foo")

    expect_equal(strsplit(capture_output(print(x)), "\n")[[1]], lines)
})


test_that("trunc 0", {
    x <- record(a = 1, b = 2, c = "foo")
    expect_equal(capture_output(print(x, 0)), "(3 entries total)")
})

test_that("trunc 1", {
    x <- record(a = 1, b = 2, c = "foo")
    expect_equal(capture_output(print(x, 1)), "a : 1\n... (3 entries total)")
})

test_that("trunc 2", {
    x <- record(a = 1, b = 2, c = "foo")
    expect_equal(capture_output(print(x, 2)),
                 "a : 1\nb : 2\n... (3 entries total)")
})

test_that("nest 2", {
    x <- record(a = record(x = record(foo = "a", bar = "baz"), y = 10))
lines <- c(
"a:",
"  x:",
"    foo : a",
"    bar : baz",
"  y     : 10")

    expect_equal(strsplit(capture_output(print(x)), "\n")[[1]], lines)
})


test_that("vector entry", {
    x <- record(a = letters)
    expect_equal(capture_output(print(x, 2)), "a : character(26)")
})


test_that("array entry", {
    x <- record(mat = matrix(1:20, 4, 5))
    expect_equal(capture_output(print(x, 2)), "mat : matrix[4, 5]")
})


test_that("function entry", {
    x <- record(foo = sin)
    expect_equal(capture_output(print(x, 2)), "foo : function")
})


test_that("trunc nested", {
    x <- record(a = record(x = record(foo = "a", bar = "baz"), y = 10))
    expect_equal(capture_output(print(x, 2)),
                 "a:\n  x:\n...   (5 entries total)")
})


test_that("with names", {
    ctype <- switch_ctype("C")
    on.exit(Sys.setlocale("LC_CTYPE", ctype), add = TRUE)

    x <- record(user_id = "kT43SxDgMGzbeXpO51f0hQ",
                review_id = "0xuZfa0t4MNWd3eIFF02ug",
                stars = 5,
                votes = record(funny = 10, cool = 0, useful = 1),
                text = "I'm a fan of soft serve ice cream and Guptill's Coney Express has delicious ice cream with many flavors.  I've tried Kurver Kreme in Colonie, Tastee Freeze in Delmar and Country Drive Inn in Clifton Park, but I think that this place has the best soft serve ice cream.  The portions are generous and the taste is very rich.  For example, the brownie sundae is decadently delicious but likely too much for one person.  They also have cupcake sundaes which I am looking to try soon!",
                date = "2009-06-09",
                business_id = "wbpbaWBfU54JbjLIDwERQA")

     lines <- c(
"user_id     : kT43SxDgMGzbeXpO51f0hQ",
"review_id   : 0xuZfa0t4MNWd3eIFF02ug",
"stars       : 5",
"votes:",
"  funny     : 10",
"  cool      : 0",
"  useful    : 1",
"text        : I'm a fan of soft serve ice cream and Guptill's Coney Express ...",
"date        : 2009-06-09",
"business_id : wbpbaWBfU54JbjLIDwERQA")

    expect_equal(strsplit(capture_output(print.record(x), width = 77),
                          "\n")[[1]],
                 lines)
})


test_that("without names", {
    ctype <- switch_ctype("C")
    on.exit(Sys.setlocale("LC_CTYPE", ctype), add = TRUE)

    x <- record(user_id = "kT43SxDgMGzbeXpO51f0hQ",
                review_id = "0xuZfa0t4MNWd3eIFF02ug",
                stars = 5,
                votes = record(funny = 10, cool = 0, useful = 1),
                text = "I'm a fan of soft serve ice cream and Guptill's Coney Express has delicious ice cream with many flavors.  I've tried Kurver Kreme in Colonie, Tastee Freeze in Delmar and Country Drive Inn in Clifton Park, but I think that this place has the best soft serve ice cream.  The portions are generous and the taste is very rich.  For example, the brownie sundae is decadently delicious but likely too much for one person.  They also have cupcake sundaes which I am looking to try soon!",
                date = "2009-06-09",
                business_id = "wbpbaWBfU54JbjLIDwERQA")
    names(x$votes) <- NULL
    names(x) <- NULL

    lines <- c(
"[[1]]   : kT43SxDgMGzbeXpO51f0hQ",
"[[2]]   : 0xuZfa0t4MNWd3eIFF02ug",
"[[3]]   : 5",
"[[4]]:",
"  [[1]] : 10",
"  [[2]] : 0",
"  [[3]] : 1",
"[[5]]   : I'm a fan of soft serve ice cream and Guptill's Coney Express has ...",
"[[6]]   : 2009-06-09",
"[[7]]   : wbpbaWBfU54JbjLIDwERQA")

    expect_equal(strsplit(capture_output(print.record(x), width = 77),
                          "\n")[[1]],
                 lines)
})
