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

split.dataset <- function(x, f, drop = FALSE, ...)
{
    x <- as.dataset(x)
    f <- as.factor(f)
    drop <- as.option(drop)

    n <- nrow(x)
    y <- split(seq_len(n), f, drop)

    for (k in seq_along(y)) {
        y[[k]] <- x[y[[k]] , , drop = FALSE]
    }

    y
}


group <- function(`_data`, ...)
{
    UseMethod("group")
}


group.default <- function(`_data`, ...)
{
    `_data` <- as.dataset(`_data`)
    by      <- substitute(cbind.dataset(...))
    by      <- eval.parent(call("scope", `_data`, by))

    group.dataset(`_data`, I(by))
}


group.dataset <- function(`_data`, ...)
{
    `_data` <- as.dataset(`_data`)
    by      <- substitute(cbind.dataset(...))
    by      <- eval.parent(call("scope", `_data`, by))

    n   <- nrow(`_data`)
    nby <- nrow(by)

    if (n != nby) {
        stop(sprintf("mismatch: data have %.0f rows, 'by' factor has %.0f",
                     n, nby))
    }

    if (n == 0) {
        return(NULL)
    }

    # split into parts
    by <- as.simple.dataset(by)
    keys(by) <- NULL

    u  <- .Call(rframe_unique, by)
    ngroup <- length(u$types)

    keys <- by[u$types, ]
    attr(keys, "keyset.type")  <- schema(keys)
    attr(keys, "keyset.hash")  <- u$hash
    attr(keys, "keyset.table") <- u$table
    class(keys) <- c("keyset", "dataset", "record")

    index <- .Call(rframe_split_group, u$group, as.double(ngroup))
    xg    <- vector("list", length(u$types))
    for (g in seq_len(ngroup)) {
        xg[[g]] <- `_data`[index[[g]], ]
    }

    y <- as.dataset(record(group = xg))
    keys(y) <- keys

    y
}
