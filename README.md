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
