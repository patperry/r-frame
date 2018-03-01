Contents
========

# Objects

## Vector-like

A *vector-like* object is an object `x` with non-negative integer `length(x)`
and `dim(x)` either `NULL` or equal to `length(x)`. This object associates
values to the integers `1, ..., n`. Index operation `x[[i]]` returns the value
for index `i`; subset operation `x[i]` returns the subset of values
corresponding the indices specified by `i`.

Examples include `list(1, 2, 3)`, `letters`, and `rnorm(10)`.


## Matrix-like

A *matrix-like* object is an object `x` with `length(dim(x)) == 2`. This
object has `n = nrow(x)` *rows* and `p = ncol(x)` *columns*. The object
associates vector-like values to the integers `1, ..., n`. Index operation
`x[i, , drop = TRUE]` returns the value for index `i`; subset operation
`x[i, , drop = FALSE]` returns the subset of values corresponding to the
indices specified by `i`.


A matrix-like object can optionally have *column names* `colnames(x)`, not
necessarily unique. The index operation `x[, j, drop = FALSE]` projects
the object onto the components specified by subset `j`. For scalar `j`,
`x[, j, drop = TRUE]` returns a vector-like object containing the values
in the `j`th component.

Examples include `matrix(1:20, 4, 5)`, `mtcars`,
`Matrix::sparseMatrix(i = c(1, 1, 2, 3, 3, 4),
                      j = c(3, 2, 1, 3, 2, 1),
                      x = c(2.8, -1.3, 7.1, 0.1, -5.1, 3.8),
                      dimnames = list(NULL, c("a", "b", "c")))`.


## Variable

A *variable with n observations* is a set of values identified with the
integers `1, ..., n`. An *atomic variable* is one that is represented as
a vector-like object. A *composite variable* is one that is represented as
a matrix-like object.


## Record

A *record* is a tuple of values. The components of a tuple can optionally have
names, and these names are not necessarily unique. We construct a record using
the `record()` function:

```r
x <- record(a = 1, b = "foo", c = FALSE)
```

When you don't specify the names in the record constructor, they get taken
from the arguments themselves:

```r
a <- 1
b <- "foo
c <- FALSE
y <- record(a, b, c)
names(y)
```

You can convert vector-like objects to records using the `as.record` function:

```
as.record(letters)
```

## Record indexing

In some ways, records behave like `list()` objects, but in other ways the two
have markedly different behavior.

### Errors for unknown names

Records error on unknown names:

```r
x["zzz"]
```

### Renaming components

Records allow you to rename the components in the result of an indexing
operations:

```r
x[c(first = "b", second = "a")]
```

This also works with integer and logical indexing:

```r
x[c(foo = 3, bar = 2, baz = 2)]
x[c(one = TRUE, two = FALSE, three = TRUE)]
```

### Printing

Records print more concisely than lists:

```r
x <- record(a = 1, b = letters, c = record(x = "nested", y = TRUE))
print(x)
```

By default, printing truncates after 20 rows. You can override this behavior
with the second argument to `print`, specifying a higher limit (or no limit
with `NA`):
```
x <- as.record(letters)
print(x) # truncates
print(x, NA) # no limit
```


## Dataset

A *dataset* is a record of zero or more variables measured on the same set of
individuals.

We construct a dataset using the `dataset()` function, analogous to the
`record()` function:

```r
data <- dataset(x = 1:10, y = letters[1:10])
```

Alternatively, we can convert another object to a `dataset` using the
`as.dataset()` function:

```r
as.dataset(mtcars)
```


## Keys

The individuals in a dataset can optionally be identified by a set of unique
values, known as *keys*. For any dataset `x`, `keys(x)` gets the associated
keys, or `NULL` if none exists. The result is a dataset with unique rows, the
same number as are in `x`.


## Simple

R `data.frames` allow only character keys. Frame `dataset` keys can have a
variety of types. We call these types *simple*. The *simple atomic* types are
`NULL`, `logical`, `integer`, `numeric`, `character`, `Date`,
`POSIXct`. A *simple* type is either a simple atomic type or a dataset of zero or
more simple types.

The `as.simple` function converts a variable to a simple variable. We can
convert many of the built-in R types, including `factor` and `complex` to
simple types using this function:

```
as.simple(factor(c("a", "a", "b", "a", "c", "a")))
as.simple(c(1+2i, 3 + 4i, -1, 6i))
```

The `as.simple` function is generic; if you add a new type to R, you can
define `as.simple` for this type.


## Setting Keys

You can set the keys of a dataset using the `keys(x)<-` function:

```
x <- as.dataset(mtcars)
keys(x) <- dataset(k1 = rnorm(32),
                   k2 = rep(c(TRUE, FALSE), 16))
```

This command converts they keys to simple, ensures that the rows are unique,
and sets the keys for `x`.

