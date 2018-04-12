arg_subscript <- function(value, n, names, get)
{
    if (missing(value) || is.null(value)) {
        return(NULL)
    }
    
    r <- length(dim(value))
    if (r >= 2) {
        stop(sprintf("subscript is a rank-%.0f array", r))
    }

    if (is.object(value)) {
        vnames <- names(value)
        if (is.numeric(value)) {
            value <- as.numeric(value)
        } else if (is.logical(value)) {
            value <- as.logical(value)
        } else {
            value <- as.character(value)
        }
        names(value) <- vnames
    }

    if (!is.character(value)) {
        return(.Call(rframe_subscript, value, n, names, get))
    } else if (is.character(value)) {
        index <- match(value, names, 0)
        index <- as.numeric(index)

        if (get) {
            vnames <- names(value)
        }

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


arg_row_subscript <- function(value, n, keys, get)
{
    if (is.record(value) || (is.list(value) && !is.object(value))) {
        value <- as.dataset(value)
        keys2 <- keys(value)
        value <- rowid(keys, value)
        if (anyNA(value)) {
            i <- which(is.na(value))[[1]]
            stop(sprintf("unknown key (row subscript %.0f)", i))
        }
    } else {
        keys2 <- NULL
        value <- arg_subscript(value, n, NULL, get)
    }

    if (get) {
        if (!is.null(keys2)) {
            keys <- keys2
        } else if (!is.null(keys)) {
            keys <- keys[value, , drop = FALSE]
            if (anyDuplicated(value)) {
                keys <- append_copy_num(keys, n, value)
            }
            keys <- as.keyset(keys)
        }
        attr(value, "keys") <- keys
    }
    value
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
