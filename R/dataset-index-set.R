`$<-.dataset` <- function(x, name, value)
{
    x[[name]] <- value
    x
}


`[[<-.dataset` <- function(x, i, value)
{
    if (!is.null(value)) {
        d <- dim(value)
        r <- length(d)
        if (r <= 1) {
            nv <- length(value)
        } else if (r == 2) {
            nv <- d[[1]]
        } else {
            stop("replacement is not a vector or matrix")
        }

        nx <- dim(x)[[1]]
        if (nx != nv) {
            fmt <- "mismatch: replacement has %.0f rows, should have %.0f"
            stop(sprintf(fmt, nv, nx))
        }
    }

    x <- NextMethod("[[<-")
    class(x) <- c("dataset", oldClass(x))
    x
}


`[<-.dataset` <- function(x, i, j, value)
{
    if (nargs() == 3L) { # x[i] <- value
        if (missing(i)) {
            replace_cols(x, NULL, value)
        } else {
            r <- length(dim(i))
            if (r <= 1) {
                replace_cols(x, i, value)
            } else if (r == 2) {
                replace_pairs(x, i, value)
            } else {
                stop(sprintf("index is a rank-%.0f array", r))
            }
        }
    } else { # x[i, j] <- value
        if (missing(i))
            i <- NULL
        if (missing(j))
            j <- NULL
        replace_block(x, i, j, value)
    }
}


replace_pairs <- function(x, pairs, value, call = sys.call(-1L))
{
    pairs <- arg_dataset_pairs_index(x, pairs, call)

    i <- pairs[, 1L, drop = TRUE]
    j <- pairs[, 2L, drop = TRUE]

    if (anyNA(i) || anyNA(j)) {
        stop(simpleError("NAs are not allowed in subscripted assignments",
                         call))
    }

    n <- length(i)
    nv <- length(value)

    if (nv == 1L) {
        value <- value[[1L]]
        for (k in seq_len(n)) {
            jk <- j[[k]]
            ik <- i[[k]]
            if (length(dim(x[[jk]])) <= 1L) {
                x[[jk]][[ik]] <- value
            } else {
                x[[jk]][ik, ] <- value
            }
        }
    } else if (nv == n) {
        for (k in seq_len(n)) {
            jk <- j[[k]]
            ik <- i[[k]]
            if (length(dim(x[[jk]])) <= 1L) {
                x[[jk]][[ik]] <- value[[k]]
            } else {
                x[[jk]][ik, ] <- value[[k]]
            }
        }
    } else {
        stop(simpleError(sprintf("number of values (%.0f) must match number of entries to replace (%.0f)", nv, n)))
    }

    x
}


replace_cols <- function(x, j, value, call = sys.call(-1L))
{
    if (!is.null(value)) {
        value <- as.dataset(value)

        nv <- nrow(value)
        nx <- nrow(x)
        if (nx != nv) {
            fmt <- "mismatch: replacement has %.0f rows, should have %.0f"
            stop(simpleError(sprintf(fmt, nv, nx), call))
        }
    }

    nrow <- attr(x, "dataset.nrow", TRUE)
    keys <- attr(x, "dataset.keys", TRUE)
    class(x) <- "record"
    x[j] <- value  # TODO: use `[<-.record` directly?
    attr(x, "dataset.keys") <- keys
    attr(x, "dataset.nrow") <- nrow
    class(x) <- c("dataset", oldClass(x))
    x
}


replace_block <- function(x, is, js, value)
{
    d <- dim(x)
    if (is.null(is)) {
        is <- seq_len(d[[1]])
    } else {
        is <- arg_dataset_row_index(x, is)
    }

    if (is.null(js)) {
        js <- seq_len(d[[2]])
    } else {
        js <- arg_subset(js, d[[2]], names(x), TRUE)
    }

    ni <- length(is)
    nj <- length(js)
    dv <- dim(value)
    rv <- length(dv)
    nv <- length(value)

    if (rv <= 1) {
        if (nv == 1) {
            rv <- 0
        } else if (nv == ni * nj) {
            rv <- 1
        } else {
            fmt <- "mistmatch: selection dimensions are %.0f x %.0f, replacement size is %.0f"
            stop(sprintf(fmt, ni, nj, nv))
        }
    } else if (rv == 2) {
        if (dv[[1]] != ni) {
            fmt <- "mistmatch: selection has %.0f rows, replacement has %.0f"
            stop(sprintf(fmt, ni, dv[[1]]))
        } else if (dv[[2]] != nj) {
            fmt <- "mistmatch: selection has %.0f columns, replacement has %.0f"
            stop(sprintf(fmt, nj, dv[[2]]))
        }
    } else {
        fmt <- "replacement must be a vector or matrix, not a rank-%.0f array"
        stop(sprintf(fmt, rv))
    }

    for (k in seq_len(nj)) {
        if (rv == 0) {
            vk <- value
        } else if (rv == 1) {
            vk <- value[(1 + (k - 1) * ni):(k * ni)]
        } else {
            vk <- value[, k, drop = TRUE]
        }
        x <- replace_subcol(x, is, js[[k]], vk)
    }

    x
}


replace_subcol <- function(x, is, j, value)
{
    if (is.null(value)) {
        if (is.null(x[[j]])) {
            return(x)
        } else {
            value <- list(NULL)
        }
    }

    xj <- x[[j]]
    if (is.null(xj)) {
        xj <- vector("list", nrow(x))
    }

    dx <- dim(xj)
    rx <- length(dx)
    dv <- dim(value)
    rv <- length(dv)
    if (rx <= 1) {
        if (rv <= 1) {
            xj[is] <- value
        } else {
            fmt <- "mismatch: destination column is a vector, replacement is a rank-%.0f array"
            stop(sprintf(fmt, rv))
        }
    } else if (rv == 2) {
        if (dx[[2]] == dv[[2]]) {
            xj[is, ] <- value
        } else {
            fmt <- "mismatch: destination column has %.0f components, replacement has %.0f"
            stop(sprintf(fmt, dx[[2]], dv[[2]]))
        }
    } else {
        fmt <- "mismatch: destination column is a matrix, replacement is a rank-%.0f array"
        stop(sprintf(fmt, rx, rv))
    }

    x[[j]] <- xj
    x
}
