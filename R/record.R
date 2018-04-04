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
    x <- list(...)
    n <- length(x)
    names <- names(x)

    if (is.null(names)) {
        names <- character(n)
        empty <- rep_len(TRUE, n)
    } else {
        empty <- !nzchar(names)
    }

    if (any(empty)) {
        args <- substitute(list(...))

        for (i in seq_len(n)) {
            if (empty[[i]]) {
                names[[i]] <- deparse(args[[i + 1L]], 500L)
            }
        }
        names(x) <- names
    }

    oldClass(x) <- "record"
    x
}


is.record <- function(x)
{
    inherits(x, "record")
}


as.list.record <- function(x, ...)
{
    attributes(x) <- list(names = names(x))
    x
}


as.record <- function(x)
    UseMethod("as.record")


as.record.default <- function(x)
{
    x <- as.list(x)
    as.record.list(x)
}


as.record.list <- function(x)
{
    if (is.object(x)) {
        x <- as.list(x)
    }
    attributes(x) <- list(names = names(x), class = "record")
    x
}


as.record.record <- function(x)
{
    x
}


`names<-.record` <- function(x, value)
{
    if (!is.null(value)) {
        n <- length(x)
        nv <- length(value)
        if (nv != n) {
            fmt <- "mismatch: `value` length is %.0f, object length is %.0f"
            stop(sprintf(fmt, nv, n))
        }
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
