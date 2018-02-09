#  Copyright 2018 Patrick O. Perry.
#
#  Licensed under the Apache License, Version 2.0 (the "License");
#  you may not use this file except in compliance with the License.
#  You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
#  Unless required by applicable law or agreed to in writing, software
#  distributed under the License is distributed on an "AS IS" BASIS,
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#  See the License for the specific language governing permissions and
#  limitations under the License.

## Records

record <- function(...)
{
# A *record* is a vector of 0 or more components, optionally with names.
# Duplicate and empty names are allowed.

    args <- substitute(list(...))[-1]

    x <- list(...)
    n <- length(x)
    names <- names(x)

    if (is.null(names)) {
        names <- character(n)
    }

    for (i in seq_len(n)) {
        if (!nzchar(names[[i]])) {
            names[[i]] <- deparse(args[[i]], 500)[[1]]
        }
    }

    names(x) <- names
    do(c.record, x)
}


is.record <- function(x, mode = NULL, n = NULL, names = NULL)
{
# The underlying storage mode for record `x` can be any list or atomic type.
# Records with components of all the same type can be stored as atomic
# vectors, but they can also be stored as lists.

    mode  <- if (is.null(mode))  "any" else as.mode(mode)
    n     <- if (is.null(n))     NA    else as.size.scalar(n)
    names <- if (is.null(names)) NULL  else as.character.vector(names, n)

    if (!is.vector(x, mode))
        FALSE
    else if (!inherits(x, "record"))
        FALSE
    else if (!is.na(n) && !isTRUE(length(x) == n))
        FALSE
    else if (!is.null(names) && !isTRUE(all(names(x) == names)))
        FALSE
    else
        TRUE
}

# We coerce other objects to records by first converting to vector. We
# preserve object names for vector inputs.

as.record <- function(x, mode = NULL, n = NULL, names = NULL)
    UseMethod("as.record")


as.record.default <- function(x, mode = NULL, n = NULL, names = NULL)
{
    mode  <- if (is.null(mode))  "any" else as.mode(mode)
    n     <- if (is.null(n))     NA    else as.size.scalar(n)
    names <- if (is.null(names)) NULL  else as.character.vector(names, n)

    if (is.record(x, mode, n, names))
        return(x)

    if (!is.na(n) && length(x) != n)
        stop("argument lengths do not match")

    if (is.null(names)) {
        names <- names(x)
    } else if (!isTRUE(all(names == names(x)))) {
        stop("argument names do not match")
    }

    # work around bug: as.character(list(NA)) == "NA"
    if (mode == "character") {
        missing <- anyNA(x)
        if (missing) {
            which <- is.na(x)
        }
    }

    x <- as.vector(x, mode)
    attributes(x) <- NULL

    if (mode == "character" && missing) {
        x[which] <- NA
    }

    class(x) <- "record"
    names(x) <- names
    x
}

as.logical.record <- function(x, n = NULL, names = NULL)
    as.record(x, "logical", n, names)
as.raw.record <- function(x, n = NULL, names = NULL)
    as.record(x, "raw", n, names)
as.integer.record <- function(x, n = NULL, names = NULL)
    as.record(x, "integer", n, names)
as.double.record <- function(x, n = NULL, names = NULL)
    as.record(x, "double", n, names)
as.complex.record <- function(x, n = NULL, names = NULL)
    as.record(x, "complex", n, names)
as.character.record <- function(x, n = NULL, names = NULL)
    as.record(x, "character", n, names)
as.list.record <- function(x, n = NULL, names = NULL)
    as.record(x, "list", n, names)

as.numeric.record <- as.double.record

as.data.frame.record <- function(x, row.names = NULL, optional = FALSE, ...)
{
    x <- as.list.record(x)
    class(x) <- NULL
    as.data.frame(x)
}

# Records mostly behave like R base vectors, but they treat the empty
# character string `""` as a missing value, and they have slightly
# different indexing (`[`) behavior.
#
# See below for a description of the indexing behavior.

anyNA.record <- function(x, recursive = FALSE)
{
    if (!is.character(x))
        NextMethod("anyNA")

    for (i in seq_along(x)) {
        elt <- x[[i]]
        if (is.na(elt) || elt == "")
            return(TRUE)
    }
    
    FALSE
}


is.na.record <- function(x)
{
    if (!is.character(x))
        NextMethod("is.na")

    n <- length(x)
    result <- logical(n)

    for (i in seq_len(n)) {
        elt <- x[[i]]
        if (is.na(elt) || elt == "")
            result[[i]] <- TRUE
    }

    result
}


# Record names are themselves records, but unlike most R objects, they
# are required to be encoded in normalized UTF-8, and they cannot contain NA.
# They are, however, allowed to contain the empty string (`""`).
#
# Note that `names(x)` may contain duplicate values.

names.record <- function(x)
    attr(x, "names_", TRUE)

`names<-.record` <- function(x, value)
{
    if (identical(names(x), value))
        return(x)

    if (!is.null(value)) {
        value <- as.character.record(value)
        value <- as.normal(value)
        n <- length(x)
        nvalue <- length(value)

        if (nvalue != n) {
            fmt <- "mismatch: `value` length is %.0f, argument length is %.0f"
            stop(sprintf(fmt, name, nvalue, n))
        }
    }

    attr(x, "names_") <- value
    x
}


c.record <- function(...)
{
    args <- list(...)
    narg <- length(args)
    mode <- modes_combine(args)
    as.record(args, mode)
}


modes_combine <- function(xlist)
{
    n <- length(xlist)
    mode <- "NULL"

    for (i in seq_len(n)) {
        x <- xlist[[i]]
        type <- typeof(x)
        if (mode == "NULL") {
            mode <- type
        } else if (type != mode) {
            return("list")
        }
    }

    if (mode == "NULL")
        "any"
    else mode
}


qualify_names <- function(n, names = NULL, prefix = NULL)
{
    if (is.null(prefix))
        names
    else if (n == 0)
        character()
    else if (!is.null(names))
        paste(prefix, names, sep = ".")
    else paste(prefix, seq_len(n), sep = ".")
}


flatten_names <- function(nlist, nameslist = NULL, prefixlist = NULL)
{
    result <- NULL

    off <- 0
    for (i in seq_along(nlist)) {
        n <- nlist[[i]]
        names <- qualify_names(n, nameslist[[i]], prefixlist[[i]])

        if (!is.null(names)) {
            if (is.null(result))
                result <- character(sum(nlist))

            if (n > 0)
                result[(off + 1):(off + n)] <- names
        }

        off <- off + n
    }

    result
}
