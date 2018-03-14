
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


key_format <- function(i)
{
    l <- as.list(i, flat = TRUE)
    s <- lapply(l, function(elt)
                if (is.logical(elt) || is.numeric(elt) || is.complex(elt))
                    as.character(elt)
                else paste0('"', as.character(elt), '"'))
    paste(s, collapse = ", ")
}
