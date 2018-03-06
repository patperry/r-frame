
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

        nx <- attr(x, "dataset.nrow", TRUE)
        if (nx != nv) {
            fmt <- "mismatch: replacement has %.0f rows, data has %.0f"
            stop(sprintf(fmt, nv, nx))
        }
    }

    x <- NextMethod("[[<-")
    class(x) <- c("dataset", oldClass(x))
    x
}


`[.dataset` <- function(x, i, j, drop = FALSE)
{
    args <- arg_dataset_index(nargs() - 1L - !missing(drop), i, j)
    drop <- as.option(drop)

    i <- args$i
    j <- args$j
    pairs <- args$pairs

    if (!is.null(j)) {
        x <- column_subset(x, j)
    }

    if (!is.null(i)) {
        x <- row_subset(x, i)
    }

    if (!is.null(pairs)) {
        return(get_pairs(x, pairs))
    }

    if (drop) {
        dim <- dim(x)
        if (!is.null(i) && dim[[1L]] == 1L) {
            x <- lapply(x, drop_row_dim)
        }
        if (!is.null(j) && dim[[2L]] == 1L) {
            x <- x[[1L]]
        }
    }

    x
}


column_subset <- function(x, i, call = sys.call(-1L))
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


row_subset <- function(x, i, call = sys.call(-1L))
{
    rows <- arg_dataset_row_index(x, i, call)
    keys <- keys(x)

    if (!is.null(keys)) {
        keys <- keys[rows, , drop = FALSE]

        if (anyDuplicated(rows)) {
            keys <- append_copy_num(keys, nrow(x), rows)
        }

        keys <- as.keyset(keys)
    }

    cols <- lapply(x, elt_subset, rows)
    attr(cols, "dataset.nrow") <- length(rows)
    attr(cols, "dataset.keys") <- keys
    class(cols) <- c("dataset", "record")

    cols
}


elt_subset <- function(x, i)
{
    if (length(dim(x)) <= 1L) {
        x[i]
    } else {
        x[i, , drop = FALSE]
    }
}


get_pairs <- function(x, pairs, call = sys.call(-1))
{
    pairs <- arg_dataset_pairs_index(x, pairs, call)

    i <- pairs[, 1L, drop = TRUE]
    j <- pairs[, 2L, drop = TRUE]

    vals <- lapply(seq_along(i), function(k) {
        jk <- j[[k]]
        if (is.na(jk)) {
            NA
        } else {
            ik <- i[[k]]
            xk <- x[[jk]]
            if (length(dim(xk)) <= 1L) {
                xk[[ik]]
            } else {
                drop_row_dim(xk[ik, , drop = FALSE])
            }
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
