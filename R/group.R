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


group <- function(x, by, do = NULL)
{
    UseMethod("group")
}


group.default <- function(x, by, do = NULL)
{
    x   <- as.dataset(x)
    by  <- as.by(x, substitute(by))
    do  <- if (!is.null(do)) as.function(do)

    group.dataset(x, I(by), do)
}


as.by <- function(x, expr)
{
    if (is.call(expr) && expr[[1]] == as.name("list")) {
        expr[[1]] <- as.name("dataset")
        eval.parent(call("scope", x, expr), n = 2)
    } else {
        if (is.call(expr) && expr[[1]] == as.name("I") && length(expr) > 1) {
            name <- deparse(expr[[2]], 500L)
        } else {
            name <- deparse(expr, 500L)
        }

        by   <- eval.parent(call("scope", x, expr), n = 2)
        if (!is.list(by) && length(dim(by)) <= 1) {
            by <- list(by)
            names(by) <- name
        }
        as.dataset(by)
    }
}


group.dataset <- function(x, by, do = NULL)
{
    x   <- as.dataset(x)
    by  <- as.by(x, substitute(by))
    do  <- if (!is.null(do)) as.function(do)

    if (nrow(by) != nrow(x)) {
        stop(sprintf("'by' rows (%.0f) must match data rows (%.0f)",
                     nrow(by), nrow(x)))
    }

    # split into parts
    keys <- as.keyset(unique.dataset(by))
    g <- lookup(by, keys)
    xg <- split.dataset(x, g) # returns a dataset
    names(xg) <- NULL

    if (length(xg) == 0L) {
        return(NULL)
    }

    y <- as.dataset(list(xg))
    keys(y) <- keys

    if (!is.null(do) && nrow(y) > 0L) {
        rows <- lapply(y[[1L]], function(g) {
            row <- do(g)
            if (!is.null(row)) {
                row <- as.dataset(row)
            }
            row
        })
        nr <- vapply(rows, NROW, 0)
        if (!all(nr == nr[[1L]])) {
            j <- which(nr != nr[[1L]])[[1L]]
            stop(sprintf("'do' action returned differing number of rows (%.0f, %.0f) for groups %.0f, %.0f (%s, %s)",
                             nrow(rows[[1]]), nrow(rows[[j]]),
                             1, j,
                             keys(y)[1,], keys(y)[j,]))
        }
        nr <- nr[[1L]]
        if (nr == 0) {
            y[] <- NULL
        } else if (nr == 1L) {
            cols <- do.call(rbind.dataset, rows)
            keys(cols) <- keys(y)
            y <- cols
        } else {
            stop(sprintf("'do' action returned multiple rows (%.0f)", nr))
        }
    }

    y
}
