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
    as.record(x)
}


is.record <- function(x, n = NULL, names = NULL)
{
    n     <- if (is.null(n))     NA   else as.size.scalar(n)
    names <- if (is.null(names)) NULL else as.character.vector(names)

    if (!is.list(x))
        FALSE
    else if (!inherits(x, "record"))
        FALSE
    else if (!is.na(n) && !isTRUE(length(x) == n))
        FALSE
    else if (!is.null(names) && !identical(names(x), names))
        FALSE
    else
        TRUE
}


as.list.record <- function(x, ...)
{
    names <- names(x)
    attributes(x) <- NULL
    names(x) <- names
    x
}


# We coerce other objects to records by first converting to list. We
# preserve object names for vector inputs.

as.record <- function(x)
    UseMethod("as.record")


as.record.record <- function(x)
{
    x
}


as.record.default <- function(x)
{
    names <- names(x)
    x <- as.list(x)
    attributes(x) <- NULL
    class(x) <- "record"
    names(x) <- names
    x
}


# Record names are themselves records, but unlike most R character objects,
# they are required to be encoded in UTF-8.
#
# Note that `names(x)` may contain duplicate values.

`names<-.record` <- function(x, value)
{
    if (identical(names(x), value))
        return(x)

    if (!is.null(value)) {
        n <- length(x)
        nv <- length(value)
        if (nv != n) {
            fmt <- "mismatch: `value` length is %.0f, object length is %.0f"
            stop(sprintf(fmt, nv, n))
        }

        value <- arg_names(value, "`value`")
    }

    attr(x, "names") <- value
    x
}

`length<-.record` <- function(x, value)
{
    x <- NextMethod("length<-")
    class(x) <- "record"
    x
}

c.record <- function(...)
{
    args <- list(...)
    nlist <- vapply(args, length, 0)
    ntot <- sum(nlist)
    result <- vector("list", ntot)
    off <- 0
    for (i in seq_along(args)) {
        n <- nlist[[i]]
        if (n > 0) {
            result[(off + 1):(off + n)] <- args[[i]]
            off <- off + n
        }
    }
    names(result) <- flatten_names(nlist, lapply(args, names), names(args))
    as.record(result)
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


qualify_names <- function(n, names = NULL, prefix = NULL)
{
    if (is.null(prefix) || !nzchar(prefix))
        names
    else if (n == 0)
        character()
    else if (!is.null(names))
        paste(prefix, names, sep = ".")
    else paste(prefix, seq_len(n), sep = ".")
}
