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

    xorig <- list(...)
    null <- vapply(xorig, is.null, NA)
    xorig <- xorig[!null]
    x <- lapply(xorig, as.dataset)
    n <- length(x)

    if (n == 0L) {
        return(NULL)
    }

    # handle named arguments
    argnames <- names(x)
    if (!is.null(argnames)) {
        for (i in seq_len(n)) {
            nm <- argnames[[i]]
            if (!nzchar(nm))
                next

            xi <- x[[i]]
            if (length(xi) == 0L)
                next

            xoi <- xorig[[i]]
            # matrix inputs (and lists) get 'name.' prefix; others just get name
            if ((is.list(xoi) && !is.object(xoi)) || (length(dim(xoi)) > 1L)) {
                ni <- names(xi)
                if (is.null(ni)) {
                    ni <- as.character(seq_along(xi))
                }
                names(xi) <- paste(nm, ni, sep = ".")
            } else {
                names(xi) <- nm
            }

            x[[i]] <- xi
        }
    }

    # get rows, columns, keys; validate
    nctot <- 0L
    keys <- NULL
    ikey <- 0L
    for (i in seq_len(n)) {
        xi <- x[[i]]
        di <- dim(xi)

        nri <- di[[1L]]
        nctot <- nctot + di[[2L]]

        if (i == 1L) {
            nr <- nri
        } else if (nri != nr) {
            index <- which(!null)
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
            index <- which(!null)
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
    for (i in seq_along(x)) {
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
    y <- as.dataset(y)
    keys(y) <- keys

    y
}


rbind.dataset <- function(..., deparse.level = 1)
{
    # ignore 'deparse.level'

    xorig <- list(...)
    null <- vapply(xorig, is.null, NA)
    xorig <- xorig[!null]
    x <- lapply(xorig, as.dataset)
    k <- lapply(x, keys)
    n <- length(x)

    if (n == 0L) {
        return(NULL)
    }

    # handle named arguments
    argnames <- names(x)
    if (!is.null(argnames)) {
        for (i in seq_len(n)) {
            nm <- argnames[[i]]
            if (!nzchar(nm))
                next

            xi <- x[[i]]
            ni <- dim(xi)[[1L]]
            if (ni == 0L)
                next

            # prepend name as first column of keys
            ki <- as.list(k[[i]])
            if (length(ki) == 0L && ni > 1L) {
                ki <- as.dataset(paste(nm, seq_len(ni), sep = "."))
            } else {
                ki <- as.dataset(c(list(rep(nm, ni)), ki))
            }
            k[[i]] <- ki
        }
    }

    # get columns
    nc <- length(x[[1L]])

    # validate columns
    for (i in seq_len(n)) {
        xi <- x[[i]]
        if (length(xi) != nc) {
            index <- which(!null)
            stop(sprintf("arguments %.0f and %.0f have different numbers of columns",
                         index[[1]], index[[i]]))
        }
    }

    # get names
    for (iname in seq_len(n)) {
        names <- names(x[[iname]])
        if (!is.null(names)) {
            break
        }
    }

    # validate names
    if (iname < n) {
        for (i in (iname + 1L):n) {
            xi <- x[[i]]
            ni <- names(xi)
            if (!is.null(ni) && !identical(ni, names)) {
                index <- which(!null)
                stop(sprintf("arguments %.0f and %.0f have different names",
                             index[[iname]], index[[i]]))
            }
        }
    }

    # get number of keys, key names
    has_keys <- !all(vapply(k, is.null, NA))
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
                index <- which(!null)
                stop(sprintf("arguments %.0f and %.0f have different numbers of keys",
                             index[[1]], index[[i]]))
            }
            nki <- names(ki)
            if (!is.null(nki) && !identical(nki, knames)) {
                index <- which(!null)
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

        if (is.null(dim(elt))) {
            col <- do.call(c, rows)
        } else {
            col <- do.call(rbind, rows)
        }

        y[[j]] <- col
    }

    y <- as.dataset(y)

    if (has_keys) {
        keys <- vector("list", nk)
        names(keys) <- knames

        if (nk == 0) {
            keys <- structure(keys, row.names = .set_row_names(nrow(y)),
                              class = "data.frame")
        } else {
            k1 <- k[[1L]]
            for (j in seq_len(nk)) {
                rows <- lapply(k, `[[`, j)
                col <- do.call(c, rows)
                keys[[j]] <- col
            }
        }

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
