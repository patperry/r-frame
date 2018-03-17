
context("dataset-print")

test_that("'print.dataset' can print all rows", {
    d <- data.frame(x = 1:50)

    expect_equal(capture_output(print(as.dataset(d), -1)),
                 capture_output(print(as.dataset(d), .Machine$integer.max)))

    expect_error(print(as.dataset(d), NA), "`rows` cannot be NA")
})


test_that("'print.dataset' produces the same results on ASCII", {
    d <- data.frame(x = 1:10, f = gl(2,5), ch = I(letters[1:10]))
    dr <- d
    dr$ch <- paste0(d$ch, " ")

    expect_equal(capture_output(print(as.dataset(d))),
                 capture_output(print(dr)))
})


test_that("'print.dataset' handles NA elements", {
    d <- data.frame(x = NA_real_, ch = I(NA_character_),
                    f = as.factor(NA_character_))
    dr <- d
    names(dr) <- c("x", "ch  ", "f   ")

    expect_equal(capture_output(print(as.dataset(d))),
                 capture_output(print(dr)))
})


test_that("'print.dataset' handles empty data frames", {
    # no row or column names
    d1 <- data.frame()
    expect_equal(capture_output(print(as.dataset(d1))),
                 "(0 rows, 0 columns)")

    # no row names
    d2 <- data.frame(a = integer(), b = integer(), "\n" = logical(),
                     check.names = FALSE)
    expect_equal(capture_output(print(as.dataset(d2))), "a b \\n\n(0 rows)")
})



test_that("'print.dataset' ignores 'right' argument", {
    d <- data.frame(ch = c("a", "ab", "abc"))

    expect_equal(capture_output(print(as.dataset(d), right = TRUE)),
                 capture_output(print(as.dataset(d), right = FALSE)))
})



test_that("'print.dataset' can wrap 4 columns", {
    ctype <- switch_ctype("C")
    on.exit(Sys.setlocale("LC_CTYPE", ctype), add = TRUE)

    x <- dataset(
        title = "The Declaration of Independence of The United States of America",
        author = "Founding Fathers",
        language = "English",
        text = "The Declaration of Independence of The United States of America\n\n\nWhen in the course of human events")

    lines <- c(
'  title                                                          ',
'1 The Declaration of Independence of The United States of America',
'.',
'  author           language text                                                ',
'1 Founding Fathers English  The Declaration of Independence of The United Sta...')

    expect_equal(strsplit(capture_output(print.dataset(x, wrap = 1),
                                         width = 80),
                          "\n")[[1]],
                 lines)
})


test_that("'print.dataset can print NA columns", {
    ctype <- switch_ctype("C")
    on.exit(Sys.setlocale("LC_CTYPE", ctype), add = TRUE)

    x <- dataset(title = c("For the Independent Journal",
                           "From the New York Packet"),
                 date = as.Date(c(NA, "1787-11-20")),
                 author = c("Hamilton", "Hamilton"),
                 text = c("To the People of the State of New York",
                          "To the People of the State of New York"))
    lines <- c(
'  title                       date       author   text                       ',
'1 For the Independent Journal <NA>       Hamilton To the People of the Sta...',
'2 From the New York Packet    1787-11-20 Hamilton To the People of the Sta...')

    expect_equal(strsplit(capture_output(print.dataset(x), width = 77),
                          "\n")[[1]], lines)
})


test_that("'print' can handle matrix columns", {
    ctype <- switch_ctype("C")
    on.exit(Sys.setlocale("LC_CTYPE", ctype), add = TRUE)

    cn <- as.character(1:13)
    rn <- as.character(1:2)
    dn <- list(rn, cn)
    x <- dataset(x = matrix(letters, 2, 13, dimnames = dn),
                 X = matrix(LETTERS, 2, 13, dimnames = dn),
                 Z = matrix(letters, 2, 13, dimnames = dn))

    lines <- c(
'  ==============x============== ==============X============== =======Z=======',
'  1 2 3 4 5 6 7 8 9 10 11 12 13 1 2 3 4 5 6 7 8 9 10 11 12 13 1 2 3 4 5 6 ...',
'1 a c e g i k m o q s  u  w  y  A C E G I K M O Q S  U  W  Y  a c e g i k ...',
'2 b d f h j l n p r t  v  x  z  B D F H J L N P R T  V  X  Z  b d f h j l ...',
'                                                           (39 columns total)')

    expect_equal(strsplit(capture_output(print(x, wrap = 0), width = 77),
                          "\n")[[1]], lines)
})


test_that("'print' can handle matrix columns with tail", {
    ctype <- switch_ctype("C")
    on.exit(Sys.setlocale("LC_CTYPE", ctype), add = TRUE)

    cn <- as.character(1:13)
    rn <- as.character(1:2)
    dn <- list(rn, cn)
    x <- dataset(x = matrix(letters, 2, 13, dimnames = dn),
                 X = matrix(LETTERS, 2, 13, dimnames = dn),
                 Z = matrix(letters, 2, 13, dimnames = dn),
                 z = c("a", "b"))

    lines <- c(
'  ==============x============== ==============X============== =====Z=====    ',
'  1 2 3 4 5 6 7 8 9 10 11 12 13 1 2 3 4 5 6 7 8 9 10 11 12 13 1 2 3 4 ... ...',
'1 a c e g i k m o q s  u  w  y  A C E G I K M O Q S  U  W  Y  a c e g ... ...',
'2 b d f h j l n p r t  v  x  z  B D F H J L N P R T  V  X  Z  b d f h ... ...',
'                                                           (40 columns total)')

    expect_equal(strsplit(capture_output(print(x, wrap = 0), width = 77),
                          "\n")[[1]], lines)
})


test_that("'print' can handle narrow grouped columns", {
    ctype <- switch_ctype("C")
    on.exit(Sys.setlocale("LC_CTYPE", ctype), add = TRUE)

    group1 <- dataset(drat = c(3.9, 3.9),
                      wt   = c(2.620, 2.875),
                      qsec = c(16.46, 17.02))
    x <- dataset(group1)

    lines <- c(
'  =====group1=====',
'  drat    wt  qsec',
'1  3.9 2.620 16.46',
'2  3.9 2.875 17.02')

    expect_equal(strsplit(capture_output(print(x)), "\n")[[1]], lines)
})


test_that("'print' can handle matrix with one column", {
    ctype <- switch_ctype("C")
    on.exit(Sys.setlocale("LC_CTYPE", ctype), add = TRUE)
              
    x <- dataset(x = matrix(5:8, 4, 1))

    lines <- c(
'  =x==',
'  [,1]',
'1    5',
'2    6',
'3    7',
'4    8')

    expect_equal(strsplit(capture_output(print(x)), "\n")[[1]], lines)
})


test_that("'print' handles single matrix with many columns", {
    ctype <- switch_ctype("C")
    on.exit(Sys.setlocale("LC_CTYPE", ctype), add = TRUE)
              
    x <- dataset(mtcars[1,])
    lines <- c(
#  00000000011111111112222222222333333333344444444445555555555666666666677777777778
#  12345678901234567890123456789012345678901234567890123456789012345678901234567890
'  =======mtcars[1, ]=======',
'  mpg cyl disp  hp drat ...',
'1  21   6  160 110  3.9 ...',
'         (11 columns total)')
    expect_equal(strsplit(capture_output(print(x, wrap = 0), width = 27),
                          "\n")[[1]],
                 lines)
})


test_that("short nested works with right-align single", {
    ctype <- switch_ctype("C")
    on.exit(Sys.setlocale("LC_CTYPE", ctype), add = TRUE)

    x <- dataset(x = c(19, 5))
    y <- dataset(long = x, short = c("z", "GG"))
    lines <- c(
'  long      ',
'   x   short',
'1 19   z    ',
'2  5   GG   ')

    expect_equal(strsplit(capture_output(print(y)), "\n")[[1]], lines)
})


test_that("short nested works with left-align single", {
    ctype <- switch_ctype("C")
    on.exit(Sys.setlocale("LC_CTYPE", ctype), add = TRUE)

    x <- dataset(x = c("aa", "b"))
    y <- dataset(long = x, short = c("z", "GG"))
    lines <- c(
'  long      ',
'  x    short',
'1 aa   z    ',
'2 b    GG   ')

    expect_equal(strsplit(capture_output(print(y)), "\n")[[1]], lines)
})


test_that("short nested works with right-align double", {
    ctype <- switch_ctype("C")
    on.exit(Sys.setlocale("LC_CTYPE", ctype), add = TRUE)

    x <- dataset(x = c(19, 5), y = c(7, 13))
    y <- dataset(really_long = x, short = c("z", "GG"))

    lines <- c(
#  12345654321
#  12345678901
'  really_long      ',
'   x  y       short',
'1 19  7       z    ',
'2  5 13       GG   ')

    expect_equal(strsplit(capture_output(print(y)), "\n")[[1]], lines)
})


test_that("two levels of nesting works", {
    ctype <- switch_ctype("C")
    on.exit(Sys.setlocale("LC_CTYPE", ctype), add = TRUE)

    set <- matrix(c(   0, -1.3, 2.8,
                     7.1,    0,   0,
                       0, -5.1, 0.1,
                     3.8,    0,   0),
                  4, 3,
                  byrow = TRUE,
                  dimnames = list(NULL, c("a", "b", "c")))
    x <- dataset(age = c(35, 70, 12, 42),
                 color = c("red", "blue", "black", "green"),
                 set = set)
    y <- dataset(value = c(1.2629543, -0.3262334, 1.3297993, 1.2724293),
                 nested = x)

lines <- c(
'             ========nested========',
'                       ====set=====',
'       value age color   a    b   c',
'1  1.2629543  35 red   0.0 -1.3 2.8',
'2 -0.3262334  70 blue  7.1  0.0 0.0',
'3  1.3297993  12 black 0.0 -5.1 0.1',
'4  1.2724293  42 green 3.8  0.0 0.0')

    expect_equal(strsplit(capture_output(print(y)), "\n")[[1]], lines)
})


test_that("printing with list", {
    ctype <- switch_ctype("C")
    on.exit(Sys.setlocale("LC_CTYPE", ctype), add = TRUE)

    x <- dataset(col = list(structure(1:4, class = "foo"),
                            structure(letters, class = "bar", dim = c(2, 13)),
                            structure(1:24, dim = c(3, 2, 4)),
                            NULL,
                            letters,
                            structure(LETTERS[1:4], dim = 4)))

    lines <- c(
'  col           ',
'1 foo(4)        ',
'2 bar[2, 13]    ',
'3 array[3, 2, 4]',
'4 NULL          ',
'5 character(26) ',
'6 array[4]      ')

    expect_equal(strsplit(capture_output(print(x)), "\n")[[1]], lines)
})


test_that("printing with key works", {
    ctype <- switch_ctype("C")
    on.exit(Sys.setlocale("LC_CTYPE", ctype), add = TRUE)

    x <- dataset(value = 1:5)
    keys(x) <- dataset(key = letters[c(5,6,1,3,12)])

    lines <- c(
'key | value',
'e   |     1',
'f   |     2',
'a   |     3',
'c   |     4',
'l   |     5')

    expect_equal(strsplit(capture_output(print(x)), "\n")[[1]], lines)
})


test_that("printing with two keys works", {
    ctype <- switch_ctype("C")
    on.exit(Sys.setlocale("LC_CTYPE", ctype), add = TRUE)

    x <- dataset(value = 1:5)
    keys(x) <- dataset(key1 = letters[c(5,6,1,3,12)],
                       key2 = c(7, 8, 100, 10, -3))

    lines <- c(
'key1 key2 | value',
'e       7 |     1',
'f       8 |     2',
'a     100 |     3',
'c      10 |     4',
'l      -3 |     5')

    expect_equal(strsplit(capture_output(print(x)), "\n")[[1]], lines)
})


test_that("printing with NA col name works", {
    x <- dataset(x = 4:6)
    names(x) <- NA
    lines <- c(
'   ',
'1 4',
'2 5',
'3 6')
    expect_equal(strsplit(capture_output(print(x)), "\n")[[1]], lines)
})


test_that("printing empty matrix", {
    x <- dataset(x = matrix(0, 5, 0))
    lines <- c(
'  x         ',
'1 numeric(0)',
'2 numeric(0)',
'3 numeric(0)',
'4 numeric(0)',
'5 numeric(0)')
    expect_equal(strsplit(capture_output(print(x)), "\n")[[1]], lines)
})
