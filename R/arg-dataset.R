
arg_dataset_row_index <- function(x, i, call = sys.call(-1L))
{
    n <- nrow(x)
    keys <- keys(x)
    
    r <- length(dim(i))
    if (is.list(i) && r <= 1L) {
        i <- as_dataset(i)
        r <- length(dim(i))
    }

    if (r <= 1L) {
        rows <- seq_len(n)
        if (is.numeric(i)) {
            # pass
        } else if (is.logical(i)) {
            if (length(i) != nrow(x)) {
                stop(simpleError(sprintf(
                     "index mask length (%.0f) must match number of rows (%.0f)",
                     length(i), nrow(x)), call))
            }
            # pass
        } else {
            rn <- rownames(x)
            if (is.null(rn)) {
                stop(simpleError(
                     "cannot index with character with 'rownames' is NULL",
                     call))
            }
            i <- as.character(i)
            names(rows) <- rn
        }

        rows <- rows[i]

    } else {
        if (is.null(keys)) {
            stop(simpleError(
                 "cannot index rows with matrix when 'keys' is NULL", call))
        }
        rows <- rowid(keys, i)
    }

    if (anyNA(rows)) {
        j <- which(is.na(rows))[[1L]]
        lab <- key_format(if (length(dim(i)) <= 1) i[[j]] else i[j,])
        stop(simpleError(sprintf(
             "selected row entry %.0f (%s) does not exist", j, lab), call))
    }

    rows
}


arg_dataset_pairs_index <- function(x, i, call = sys.call(-1L))
{
    nr <- nrow(x)
    nc <- ncol(x)
    nel <- nr * nc

    i <- as.matrix(i)
    d <- dim(i)
    d1 <- d[[1L]]
    d2 <- d[[2L]]

    if (is.logical(i)) {
        if (d2 == 1L) {
            i <- i[, 1L, drop = TRUE]
            if (length(i) != nel) {
                if (length(i) == 1L) {
                    i <- rep(i, nel)
                } else {
                    stop(simpleError(sprintf("selection mask length (%.0f) must equal number of elements (%.0f)", length(i), nel), call))
                }
            }
        } else if (d1 == nr && d2 == nc) {
            i <- as.logical(i)
        } else {
            stop(simpleError(sprintf("selection mask dimensions (%.0f, %.0f) must match data dimensions (%.0f, %.0f)", d1, d2, nr, nc), call))
        }

        i <- seq_len(nel)[as.logical(i)]
        vec <- TRUE
    } else if (d2 == 1L) {
        i <- trunc(as.numeric(i))
        bounds <- which(!(is.na(i) | (1L <= i & i <= nel)))
        if (length(bounds) > 0L) {
            b <- bounds[[1L]]
            stop(simpleError(sprintf("index %.0f (%.0f) is out of bounds",
                                     b, i[[b]]), call))
        }
        vec <- TRUE
    } else if (d2 == 2L) {
        row <- trunc(as.numeric(i[, 1L, drop = TRUE]))
        col <- trunc(as.numeric(i[, 2L, drop = TRUE]))

        bounds <- which(!((is.na(row) | (1L <= row & row <= nr))
                          & (is.na(col) | (1L <= col & col <= nc))))
        if (length(bounds) > 0L) {
            b <- bounds[[1L]]
            stop(simpleError(sprintf("index %.0f (%.0f, %.0f) is out of bounds",
                                     b, row[[b]], col[[b]]), call))
        }

        row[is.na(col)] <- NA
        col[is.na(row)] <- NA
        vec <- FALSE
    } else {
        stop(simpleError(sprintf("cannot index with %.0f-column matrix",
                                 d2), call))
    }

    if (vec) {
        if (nr > 0L) {
            i0 <- i - 1L
            row <- i0 %% nr + 1L
            col <- i0 %/% nr + 1L
        } else {
            row <- col <- rep(NA_integer_, length(i))
        }
    }

    cbind(row, col)
}


key_format <- function(i)
{
    l <- as.list(i, flat = TRUE)
    s <- lapply(l, function(elt)
                if (is.logical(elt) || is.numeric(elt) || is.complex(elt))
                    as.character(elt)
                else paste0('"', as.character(elt), '"'))
    paste(s, collapse = ", ")
}
