<!-- README.md is generated from README.Rmd. Please edit that file -->



frame
=====

[![Build Status (Linux)][travis-badge]][travis]
[![Build Status (Windows)][appveyor-badge]][appveyor]
[![Coverage Status][codecov-badge]][codecov]
[![CRAN Status][cran-badge]][cran]
[![License][apache-badge]][apache]
[![CRAN RStudio Mirror Downloads][cranlogs-badge]][cran]


*frame* is an R package providing a `dataset` type analogous to `data.frame`
that allows you to keep track of the context associated with the values by
specifying a single- or multi-component key for each row.


The package is built around the idea that a data point consists of a
(variable, key, value) triple identifying an attribute, target, and value.
This notion of data differs from Wickham's notion of "tidy" data, which allows
only (variable, value) pairs.  Having explicit support for keys makes it
easier to link different measurements made on the same set of individuals and
makes it easier to identify the sources giving rise to downstream results.  R
`data.frame` objects have partial support for keys through their `rownames`;
the `dataset` object extends this support by allowing non-character and
multi-component keys.


Installation
------------

### Stable version

*frame* is [available on CRAN][cran]. To install the latest released version,
run the following command in R:

```r
### install.packages("frame") # not yet, actually
```

### Development version

To install the latest development version, run the following:

```r
devtools::install_github("patperry/r-frame")
```


Usage
-----

### Datasets

The `dataset` type is like a `data.frame` but it allows matrix-like columns,
including sparse matrices and nested datasets.


```r
# dataset with a sparse matrix column
(x <- dataset(age = c(35, 70, 12, 42),
              color = c("red", "blue", "black", "green"),
              set = Matrix::sparseMatrix(i = c(1, 1, 2, 3, 3, 4),
                                         j = c(3, 2, 1, 3, 2, 1),
                                         x = c(2.8, -1.3, 7.1, 0.1, -5.1, 3.8),
                                         dimnames = list(NULL, c("a", "b", "c")))))
#>             ════set═════
#>   age color   a    b   c
#> 1  35 red   0.0 -1.3 2.8
#> 2  70 blue  7.1  0.0 0.0
#> 3  12 black 0.0 -5.1 0.1
#> 4  42 green 3.8  0.0 0.0

# dataset with a dataset column
(y <- dataset(value = rnorm(4), nested = x))
#>              ════════nested════════
#>                        ════set═════
#>        value age color   a    b   c
#> 1  1.2629543  35 red   0.0 -1.3 2.8
#> 2 -0.3262334  70 blue  7.1  0.0 0.0
#> 3  1.3297993  12 black 0.0 -5.1 0.1
#> 4  1.2724293  42 green 3.8  0.0 0.0

# convert a data.frame
as.dataset(mtcars[1:5, ])
#>    mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> 1 21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
#> 2 21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
#> 3 22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
#> 4 21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
#> 5 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
```

### Keys

Datasets can have multi-component keys that uniquely identify each row.


```r
# set single-component keys
keys(y) <- c("w", "x", "y", "z")

# set multi-component keys
keys(x) <- keyset(major = c("x", "x", "y", "y"),
                  minor = c(1, 2, 1, 3))

# show the data keys and values
print(x)
#>                         ════set═════
#> major minor │ age color   a    b   c
#> x         1 │  35 red   0.0 -1.3 2.8
#> x         2 │  70 blue  7.1  0.0 0.0
#> y         1 │  12 black 0.0 -5.1 0.1
#> y         3 │  42 green 3.8  0.0 0.0
```

### Indexing and slicing

Index a dataset just like a `data.frame`, or use key values to index or slice.


```r
# index with keys
x[dataset(major = c("y", "x"),
          minor = c(  3,   1)), ]
#>                         ════set═════
#> major minor │ age color   a    b   c
#> y         3 │  42 green 3.8  0.0 0.0
#> x         1 │  35 red   0.0 -1.3 2.8
```

### Grouping

Split the rows according to groups defined by one or more columns, optionally
performing a computation on each group.


```r
# split the rows into groups defined by unique ('cyl', 'gear') combinations;
# the grouping factors are the keys for the result
xg <- group(mtcars, cyl, gear)

# perform a computation on all groups
do(xg, function(x)
   record(n   = nrow(x),
          mpg = mean(x$mpg),
          hp  = mean(x$hp)))
#> cyl gear │  n    mpg       hp
#>   6    4 │  4 19.750 116.5000
#>   4    4 │  8 26.925  76.0000
#>   6    3 │  2 19.750 107.5000
#>   8    3 │ 12 15.050 194.1667
#>   4    3 │  1 21.500  97.0000
#>   4    5 │  2 28.200 102.0000
#>   8    5 │  2 15.400 299.5000
#>   6    5 │  1 19.700 175.0000
```


Citation
--------

Cite *frame* with the following BibTeX entry:

    @Manual{,
        title = {frame: Data with Context},
        author = {Patrick O. Perry},
        year = {2018},
        note = {R package version 0.0.0},
    }


Contributing
------------

The project maintainer welcomes contributions in the form of feature requests,
bug reports, comments, unit tests, vignettes, or other code.  If you'd like to
contribute, either

 + fork the repository and submit a pull request (note the nonstandard
   instructions for [building from source][building]);

 + [file an issue][issues];

 + or contact the maintainer via e-mail.

This project is released with a [Contributor Code of Conduct][conduct],
and if you choose to contribute, you must adhere to its terms.


[apache]: https://www.apache.org/licenses/LICENSE-2.0.html "Apache License, Version 2.0"
[apache-badge]: https://img.shields.io/badge/License-Apache%202.0-blue.svg "Apache License, Version 2.0"
[appveyor]: https://ci.appveyor.com/project/patperry/r-frame/branch/master "Continuous Integration (Windows)"
[appveyor-badge]: https://ci.appveyor.com/api/projects/status/github/patperry/r-frame?branch=master&svg=true "Continuous Inegration (Windows)"
[building]: #development-version "Building from Source"
[codecov]: https://codecov.io/github/patperry/r-frame?branch=master "Code Coverage"
[codecov-badge]: https://codecov.io/github/patperry/r-frame/coverage.svg?branch=master "Code Coverage"
[conduct]: https://github.com/patperry/r-frame/blob/master/CONDUCT.md "Contributor Code of Conduct"
[cran]: https://cran.r-project.org/package=frame "CRAN Page"
[cran-badge]: http://www.r-pkg.org/badges/version/frame "CRAN Page"
[cranlogs-badge]: http://cranlogs.r-pkg.org/badges/frame "CRAN Downloads"
[issues]: https://github.com/patperry/r-frame/issues "Issues"
[travis]: https://travis-ci.org/patperry/r-frame "Continuous Integration (Linux)"
[travis-badge]: https://api.travis-ci.org/patperry/r-frame.svg?branch=master "Continuous Integration (Linux)"
