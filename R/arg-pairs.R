
arg_pairs <- function(i, dim)
{
    nr <- dim[[1]]
    nc <- dim[[2]]
    nel <- nr * nc

    i <- as.matrix(i)
    d <- dim(i)
    d1 <- d[[1]]
    d2 <- d[[2]]

    if (is.logical(i)) {
        if (d2 == 1L) {
            i <- i[, 1L, drop = TRUE]
            if (length(i) != nel) {
                if (length(i) == 1L) {
                    i <- rep(i, nel)
                } else {
                    stop(sprintf("selection mask length (%.0f) must equal number of elements (%.0f)",
                                 length(i), nel))
                }
            }
        } else if (d1 == nr && d2 == nc) {
            i <- as.logical(i)
        } else {
            stop(sprintf("selection mask dimensions (%.0f, %.0f) must match data dimensions (%.0f, %.0f)",
                         d1, d2, nr, nc))
        }

        i <- seq_len(nel)[as.logical(i)]
        vec <- TRUE
    } else if (d2 == 1L) {
        i <- trunc(as.numeric(i))
        bounds <- which(!(is.na(i) | (1L <= i & i <= nel)))
        if (length(bounds) > 0L) {
            b <- bounds[[1L]]
            stop(sprintf("index %.0f (%.0f) is out of bounds", b, i[[b]]))
        }
        vec <- TRUE
    } else if (d2 == 2L) {
        row <- trunc(as.numeric(i[, 1L, drop = TRUE]))
        col <- trunc(as.numeric(i[, 2L, drop = TRUE]))

        bounds <- which(!((is.na(row) | (1L <= row & row <= nr))
                          & (is.na(col) | (1L <= col & col <= nc))))
        if (length(bounds) > 0L) {
            b <- bounds[[1L]]
            stop(sprintf("index %.0f (%.0f, %.0f) is out of bounds",
                         b, row[[b]], col[[b]]))
        }

        row[is.na(col)] <- NA
        col[is.na(row)] <- NA
        vec <- FALSE
    } else {
        stop(sprintf("cannot index with %.0f-column matrix", d2))
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
