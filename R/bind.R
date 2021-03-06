#  Copyright 2017 Patrick O. Perry.
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

cbind.dataset <- function(..., deparse.level = 1)
{
    # ignore 'deparse.level' argument

    exprs    <- substitute(list(...))
    args     <- list(...)
    narg     <- length(args)
    argnames <- names(args)

    x     <- vector("list", narg)
    index <- integer(narg)
    n     <- 0
    nctot <- 0

    # convert arguments to dataset and set names
    for (i in seq_len(narg)) {
        col <- args[[i]]
        if (is.null(col))
            next

        vec <- (length(dim(col)) <= 1 && !is.record(col))
        col <- as.dataset(col)

        if (vec) {
            if (is.null(argnames) || !nzchar(argnames[[i]])) {
                names(col) <- deparse(exprs[[i + 1]], 500L)
            } else {
                names(col) <- argnames[[i]]
            }
            nctot <- nctot + 1
        } else {
            nc    <- length(col)
            nctot <- nctot + nc

            if (!is.null(argnames) && nzchar(argnames[[i]]) && nc > 0) {
                prefix <- argnames[[i]]
                suffix <- names(col)
                if (is.null(suffix)) {
                    suffix <- as.character(seq_along(col))
                } else {
                    miss <- which(is.na(suffix) | !nzchar(suffix))
                    suffix[miss] <- as.character(miss)
                }
                names(col) <- paste(prefix, suffix, sep = ".")
            }
        }

        n          <- n + 1
        index[[n]] <- i
        x[[n]]     <- col
    }

    if (n == 0) {
        return(NULL)
    }

    # get rows, columns, keys; validate
    keys  <- NULL
    ikey  <- 0L

    for (i in seq_len(n)) {
        xi <- x[[i]]
        di <- dim(xi)

        nri   <- di[[1L]]

        if (i == 1L) {
            nr <- nri
        } else if (nri != nr) {
            stop(sprintf("mismatch: argument %.0f has %.0f rows, argument %.0f has %.0f",
                         index[[1L]], nr, index[[i]], nri))
        }

        ki <- keys(xi)
        if (is.null(ki)) {
            next
        }
        if (ikey == 0L) {
            keys <- ki
            ikey <- i
        } else if (!identical(ki, keys)) {
            stop(sprintf("arguments %.0f and %.0f have different keys",
                         index[[ikey]], index[[i]]))
        }
    }

    if (nctot == 0L) {
        x1 <- x[[max(1L, ikey)]]
        return(x1)
    }

    y <- vector("list", nctot)
    names <- NULL

    off <- 0L
    for (i in seq_len(n)) {
        xi <- x[[i]]
        nc <- ncol(xi)

        ix <- off + seq_len(nc)
        y[ix] <- xi

        ni <- names(xi)
        if (!is.null(ni)) {
            if (is.null(names)) {
                names <- character(nctot)
            }
            names[ix] <- ni
        }

        off <- off + nc
    }

    names(y) <- names
    y <- as.record(y)
    y <- as.dataset(y)
    keys(y) <- keys

    y
}


rbind.dataset <- function(..., deparse.level = 1)
{
    # ignore 'deparse.level'

    args     <- list(...)
    narg     <- length(args)
    argnames <- names(args)

    if (!is.null(argnames)) {
        stop("named arguments are not allowed")
    }

    x        <- vector("list", narg)
    k        <- vector("list", narg)
    index    <- integer(narg)
    n        <- 0L
    has_keys <- FALSE

    for (i in seq_len(narg)) {
        row <- args[[i]]
        if (is.null(row))
            next

        if (length(dim(row)) <= 1) {
            row <- as.record(row)
        }
        row <- as.dataset(row)

        n          <- n + 1L
        x[[n]]     <- row
        index[[n]] <- i
        keys <- keys(row)
        if (!is.null(keys)) {
            has_keys <- TRUE
            k[[n]] <- keys
        }
    }

    if (n == 0L) {
        return(NULL)
    }

    # get number of columns, names
    nc <- length(x[[1]])
    for (iname in seq_len(n)) {
        names <- names(x[[iname]])
        if (!is.null(names)) {
            break
        }
    }

    # validate columns
    for (i in seq_len(n)) {
        xi <- x[[i]]
        if (length(xi) != nc) {
            stop(sprintf("arguments %.0f and %.0f have different numbers of columns",
                         index[[1]], index[[i]]))
        }

        ni <- names(xi)
        if (!is.null(ni) && !identical(ni, names)) {
            stop(sprintf("arguments %.0f and %.0f have different names",
                         index[[iname]], index[[i]]))
        }
    }

    # get number of keys, key names
    if (has_keys) {
        nk <- length(k[[1]])
        for (ikname in seq_len(n)) {
            ki <- k[[ikname]]
            knames <- names(ki)
            if (!is.null(knames)) {
                break
            }
        }

        # validate keys
        for (i in seq_len(n)) {
            ki <- k[[i]]
            if (length(ki) != nk) {
                stop(sprintf("arguments %.0f and %.0f have different numbers of keys",
                             index[[1]], index[[i]]))
            }
            nki <- names(ki)
            if (!is.null(nki) && !identical(nki, knames)) {
                stop(sprintf("arguments %.0f and %.0f have different key names",
                             index[[ikname]], index[[i]]))
            }
        }
    }

    y <- vector("list", nc)
    names(y) <- names
    x1 <- x[[1L]]

    for (j in seq_len(nc)) {
        elt <- x1[[j]]
        rows <- lapply(x, `[[`, j)

        if (length(dim(elt)) <= 1) {
            col <- do.call(c, rows)
        } else {
            col <- do.call(rbind, rows)
        }

        y[[j]] <- col
    }

    y <- as.record(y)
    y <- as.dataset(y)

    if (has_keys) {
        if (nk == 0) {
            keys <- as.dataset(matrix(0, nrow(y), 0))
        } else {
            keys <- do.call(rbind.dataset, k)
        }
        names(keys) <- knames
        keys(y) <- make_unique(keys)
    }

    y
}


make_unique <- function(x)
{
    x <- as.dataset(x)
    if (!anyDuplicated(x)) {
        return(as.keyset(x))
    }

    keys <- unique(x)
    nkey <- nrow(keys)
    id <- lookup(x, keys)

    y <- append_copy_num(x, nkey, id)
    as.keyset(y)
}
