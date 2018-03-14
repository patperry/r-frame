
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
                stop(sprintf("index is a rank-%.0f array", r))
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
    nrow <- attr(x, "dataset.nrow", TRUE)
    keys <- attr(x, "dataset.keys", TRUE)
    
    if (!is.null(i)) {
        i <- arg_index(i, nrow, NULL, TRUE)

        if (!is.null(keys)) {
            keys <- keys[i, , drop = FALSE]

            if (anyDuplicated(i)) {
                keys <- append_copy_num(keys, nrow, i)
            }

            keys <- as.keyset(keys)
        }

        nrow <- length(i)
        x <- lapply(x, elt_subset, i)
    } else {
        names <- names(x)
        attributes(x) <- NULL
        names(x) <- names
    }

    attr(x, "dataset.nrow") <- nrow
    attr(x, "dataset.keys") <- keys
    class(x) <- c("dataset", "record")

    x
}


append_copy_num <- function(x, nkey, id)
{
    # TODO: implement in C?
    copy <- integer(nkey)
    newkey <- integer(length(id))
    for (i in seq_along(id)) {
        k <- id[[i]]
        copy[[k]] <- copy[[k]] + 1L
        newkey[[i]] <- copy[[k]]
    }
    names <- names(x)
    if (is.null(names)) {
        names <- character(length(x))
    }

    x[[length(x) + 1L]] <- newkey
    names(x) <- c(names, "#")

    x
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
