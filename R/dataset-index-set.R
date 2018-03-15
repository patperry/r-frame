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
                stop(sprintf("subscript is a rank-%.0f array", r))
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


replace_pairs <- function(x, pairs, value)
{
    pairs <- arg_pairs(pairs, dim(x))

    i <- pairs[, 1L, drop = TRUE]
    j <- pairs[, 2L, drop = TRUE]

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


replace_cols <- function(x, j, value)
{
    if (!is.null(value)) {
        value <- as.dataset(value)

        nv <- nrow(value)
        nx <- nrow(x)
        if (nx != nv) {
            fmt <- "mismatch: replacement has %.0f rows, should have %.0f"
            stop(sprintf(fmt, nv, nx))
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
        is <- arg_subscript(is, d[[1]], NULL, TRUE)
    }

    if (is.null(js)) {
        js <- seq_len(d[[2]])
    } else {
        js <- arg_subscript(js, d[[2]], names(x), TRUE)
    }

    ni <- length(is)
    nj <- length(js)

    if (is.null(value)) {
        rv <- 0
    } else {
        value <- as.dataset(value)
        dv <- dim(value)
        nv <- dv[[1]] * dv[[2]]

        if (dv[[1]] == ni && dv[[2]] == nj) {
            rv <- 2
        } else if (nv == 1) {
            rv <- 0
            value <- value[[1]][[1]]
        } else if (dv[[2]] == 1 && nv == ni * nj) {
            rv <- 1
            value <- value[[1]]
        } else {
            fmt <- "mismatch: replacement dimensions are %.0f x %.0f, should be %.0f x %.0f"
            stop(sprintf(fmt, dv[[1]], dv[[2]], ni, nj))
        }
    }

    for (k in seq_len(nj)) {
        if (rv == 0) {
            vk <- value
        } else if (rv == 1) {
            vk <- value[(1 + (k - 1) * ni):(k * ni)]
        } else {
            vk <- value[[k]]
        }
        x <- replace_subcol(x, is, js[[k]], vk)
    }

    x
}


replace_subcol <- function(x, is, j, value)
{
    xj <- x[[j]]
    if (is.null(xj)) {
        if (is.null(value)) {
            return(x)
        } else {
            xj <- vector("list", nrow(x))
        }
    }

    dx <- dim(xj)
    rx <- length(dx)

    if (rx <= 1) {
        xj[is] <- value
    } else {
        xj[is, ] <- value
    }

    x[[j]] <- xj
    x
}
