arg_subscript <- function(value, n, names, get)
{
    if (missing(value) || is.null(value)) {
        return(NULL)
    }
    
    r <- length(dim(value))
    if (r >= 2) {
        stop(sprintf("subscript is a rank-%.0f array", r))
    }

    if (get) {
        vnames <- names(value)
        if (!is.null(vnames)) {
            vnames <- arg_names(vnames, "replacement name")
        }
    }
    
    if (is.object(value)) {
        if (is.numeric(value)) {
            value <- as.numeric(value)
        } else if (is.logical(value)) {
            value <- as.logical(value)
        } else {
            value <- as.character(value)
        }
        if (get) {
            names(value) <- vnames
        }
    }

    if (is.numeric(value)) {
        if (anyNA(value)) {
            stop("numeric subscript cannot contain NA values")
        }

        if (length(value) == 0) {
            # pass
        } else if (isTRUE(value[[1]] < 0)) {
            if (!all(value < 0)) {
                stop("numeric subscript cannot contain both negative and non-negative values")
            }
            value <- seq_len(n)[value]
        } else {
            if (!all(value >= 0)) {
                stop("numeric subscript cannot contain both negative and non-negative values")
            }

            if (get) {
                bounds <- value > n
                if (any(bounds)) {
                    i <- which(bounds)[[1]]
                    vi <- value[[i]]
                    fmt <- "bounds error: index is %.0f, maximum is %.0f"
                    stop(simpleError(sprintf(fmt, vi, n), call))
                }
            }
        }
    } else if (is.logical(value)) {
        nvalue <- length(value)
        if (nvalue != n) {
            fmt <- "mismatch: logical subscript length is %.0f, should be %.0f"
            stop(simpleError(sprintf(fmt, nvalue, n), call))
        }

        value <- which(value)
        if (get) {
            names(value) <- vnames[value]
        }
    } else if (is.character(value)) {
        value <- arg_names(value, "index")
        index <- match(value, names, 0)

        if (!get || is.null(vnames)) {
            vnames <- value
        } else {
            empty <- is.na(vnames) | !nzchar(vnames)
            vnames[empty] <- value[empty]
        }
        names(index) <- vnames

        new <- which(index == 0)
        nnew <- length(new)
        if (nnew > 0) {
            if (get) {
                i <- new[[1]]
                vi <- value[[i]]
                if (is.na(vi)) {
                    stop("unknown name <NA>")
                } else {
                    stop(sprintf("unknown name \"%s\"", vi))
                }
            } else {
                inew <- (n + 1L):(n + nnew)
                vnew <- value[new]
                index[new] <- inew[match(vnew, vnew, 0)] # handle duplicates
            }
        }
        value <- index
    }

    value
}
