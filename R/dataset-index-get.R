
`[.dataset` <- function(x, i, j, drop = FALSE)
{
    nargs <- nargs() - !missing(drop)
    drop <- as.option(drop)

    if (nargs == 2L) { # x[i]
        if (missing(i) || is.null(i)) {
            x <- column_subset(x, NULL)
        } else {
            r <- length(dim(i))
            if (r <= 1) {
                x <- column_subset(x, i)
            } else if (r == 2) {
                return(get_pairs(x, i))
            } else {
                stop(sprintf("subscript is a rank-%.0f array", r))
            }
            if (drop && length(x) == 1)
                x <- x[[1]]
        }

    } else { # x[i, j]
        if (missing(i))
            i <- NULL
        if (missing(j))
            j <- NULL

        x <- column_subset(x, j)
        x <- row_subset(x, i)

        if (drop) {
            dim <- dim(x)
            if (!is.null(i) && dim[[1]] == 1) {
                x <- lapply(x, drop_row_dim)
                x <- as.record(x)
            }
            if (!is.null(j) && dim[[2]] == 1) {
                x <- x[[1]]
            }
        }
    }

    x
}


column_subset <- function(x, i)
{
    nrow <- attr(x, "dataset.nrow", TRUE)
    keys <- attr(x, "dataset.keys", TRUE)
    class(x) <- "record"
    x <- x[i] # TODO: use `[.record` directly?
    attr(x, "dataset.nrow") <- nrow
    attr(x, "dataset.keys") <- keys
    class(x) <- c("dataset", oldClass(x))
    x
}


row_subset <- function(x, i)
{
    nrow <- attr(x, "dataset.nrow", TRUE)
    keys <- attr(x, "dataset.keys", TRUE)
    
    if (is.null(i)) {
        attributes(x) <- list(names = names(x),
                              dataset.nrow = nrow,
                              dataset.keys = keys,
                              class = c("dataset", "record"))
        return(x)
    }

    i    <- arg_row_subscript(i, nrow, keys, TRUE)
    keys <- attr(i, "keys", TRUE)
    nrow <- length(i)
    n    <- length(x)

    y <- vector("list", n)

    for (j in seq_len(n)) {
        xj <- x[[j]]
        if (is.null(xj)) {
            # pass
        } else if (length(dim(xj)) <= 1) {
            if (is.object(xj)) {
                y[[j]] <- xj[i]
            } else {
                y[[j]] <- .Call(rframe_subset, xj, i)
            }
        } else {
            y[[j]] <- xj[i, , drop = FALSE]
        }
    }

    names(y) <- names(x)
    x <- y

    attr(x, "dataset.nrow") <- nrow
    attr(x, "dataset.keys") <- keys
    class(x) <- c("dataset", "record")

    x
}


elt_subset <- function(elt, i)
{
    if (length(dim(elt)) <= 1) {
        if (is.object(elt)) {
            elt[i]
        } else {
            i <- as.numeric(i)
        }
    } else {
        elt[i, , drop = FALSE]
    }
}


get_pairs <- function(x, pairs)
{
    pairs <- arg_pairs(pairs, dim(x))

    i <- pairs[, 1L, drop = TRUE]
    j <- pairs[, 2L, drop = TRUE]

    vals <- lapply(seq_along(i), function(k) {
        ik <- i[[k]]
        jk <- j[[k]]
        xk <- x[[jk]]
        if (length(dim(xk)) <= 1L) {
            xk[[ik]]
        } else {
            drop_row_dim(xk[ik, , drop = FALSE])
        }
    })

    vals
}


drop_row_dim <- function(x)
{
    dim <- dim(x)
    if (length(dim) < 2L) {
        x[[1L]]
    } else if (is.data.frame(x)) {
        # can't use x[1L, , drop = TRUE] for data.frame since that does
        # not return a list when x has 0 or 1 column
        lapply(x, drop_row_dim)
    } else {
        x[1L, , drop = TRUE]
    }
}
