
arg_pairs <- function(value, dim)
{
    value <- as.matrix(value)
    d <- dim(value)
    d1 <- d[[1]]
    d2 <- d[[2]]

    nr <- dim[[1]]
    nc <- dim[[2]]
    nel <- nr * nc

    if (is.logical(value)) {
        if (d2 == 1) {
            value <- value[, 1, drop = TRUE]
            if (d1 != nel) {
                if (d1 == 1) {
                    value <- rep_len(value, nel)
                } else {
                    stop(sprintf("mismatch: subscript length is %.0f, should be %.0f",
                                 d1, nel))
                }
            }
        } else if (d1 == nr && d2 == nc) {
            value <- as.logical(value)
        } else {
            stop(sprintf("mismatch: subscript dimensions are %.0f x %.0f, should be %.0f x %.0f",
                         d1, d2, nr, nc))
        }

        i <- which(value)
        vec <- TRUE
    } else if (d2 == 1L) {
        i <- trunc(as.numeric(value))
        bounds <- which(is.na(i) | !(1L <= i & i <= nel))
        if (length(bounds) > 0L) {
            b <- bounds[[1L]]
            stop(sprintf("subscript entry %.0f is invalid (%.0f)", b, i[[b]]))
        }
        vec <- TRUE
    } else if (d2 == 2L) {
        row <- trunc(as.numeric(value[, 1L, drop = TRUE]))
        col <- trunc(as.numeric(value[, 2L, drop = TRUE]))

        bounds <- which(is.na(row)
                        | !(1L <= row & row <= nr)
                        | is.na(col)
                        | !(1L <= col & col <= nc))
        if (length(bounds) > 0L) {
            b <- bounds[[1L]]
            stop(sprintf("subscript row %.0f is invalid (%.0f, %.0f)",
                         b, row[[b]], col[[b]]))
        }

        vec <- FALSE
    } else {
        stop(sprintf("invalid matrix subscript (%.0f columns)", d2))
    }

    if (vec) {
        i0 <- i - 1L
        row <- i0 %% nr + 1L
        col <- i0 %/% nr + 1L
    }

    cbind(row, col)
}
