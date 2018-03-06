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
    args <- arg_dataset_index(nargs() - 2L, i, j)
    i <- args$i
    j <- args$j
    pairs <- args$pairs

    if (!is.null(pairs)) {
        replace_pairs(x, pairs, value)
    } else if (is.null(i)) {
        replace_cols(x, j, value)
    } else {
        replace_cells(x, i, j, value)
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

    class(x) <- "record"
    x[j] <- value
    class(x) <- c("dataset", oldClass(x))
    x
}


as_column <- function(x, n)
{
    # promote scalars
    if (length(x) == 1 && length(dim(x)) <= 1) {
        x <- rep(x, n)
    }

    # drop keys
    if (!is.null(keys(x))) {
        keys(x) <- NULL
    }

    # drop names
    d <- dim(x)
    if (is.null(d)) {
        names(x) <- NULL
    } else if (length(d) == 1) {
        dimnames(x) <- NULL
    } else {
        rownames(x) <- NULL
    }

    x
}


replace_cells <- function(x, i, j, value, call = sys.call(-1L))
{
    if (is.null(i)) {
        i <- seq_len(nrow(x))
    } else {
        i <- arg_row_index(x, i, call)
    }
    if (is.null(j)) {
        j <- seq_along(x)
    } else {
        j <- arg_col_index(x, j, call)
    }

    ni <- length(i)
    nj <- length(j)
    recycle <- arg_dataset_recycle(ni, nj, value, call)$cols

    if (ni == 0 || nj == 0) {
        return(x)
    }

    for (k in seq_along(j)) {
        jk <- j[[k]]
        vk <- if (recycle) value[[1L]] else value[[k]]

        if (is.null(x[[jk]])) {
            xjk <- vector("list", dim(x)[[1L]])
            if (!is.null(vk)) {
                xjk[[i]] <- vk
            }
            x[[jk]] <- xjk
        } else {
            if (length(dim(x[[jk]])) <= 1L) {
                x[[jk]][i] <- vk
            } else {
                x[[jk]][i,] <- vk
            }
        }
    }

    x
}


arg_dataset_recycle <- function(ni, nj, value, call = sys.call(-1L))
{
    dim <- dim(value)
    if (is.null(dim)) {
        rows <- length(value)
        if (rows == ni * nj) {
            return(list(rows = FALSE, cols = FALSE))
        }
        cols <- 1L
    } else {
        rows <- dim[[1L]]
        cols <- dim[[2L]]
    }

    if (rows == ni) {
        recycle_rows <- FALSE
    } else if (rows == 1L) {
        recycle_rows <- TRUE
    } else {
        stop(simpleError(sprintf(
             "replacement has %.0f rows, need %.0f", rows, ni), call))
    }

    if (cols == nj) {
        recycle_cols <- FALSE
    } else if (cols == 1L) {
        recycle_cols <- TRUE
    } else {
        stop(simpleError(sprintf(
             "replacement has %.0f columns, need %.0f", cols, nj), call))
    }

    list(rows = recycle_rows, cols = recycle_cols)
}
