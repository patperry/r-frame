sort.dataset <- function(x, decreasing = FALSE, ...)
{
    x <- as.dataset(x)
    decreasing <- as.option(decreasing)
    y <- xtfrm(x)
    o <- order(y, decreasing = decreasing)
    x[o, ]
}


xtfrm.dataset <- function(x)
{
    x <- as.dataset(x)
    n <- length(x)

    null <- logical(n)

    for (i in seq_along(x)) {
        elt <- x[[i]]
        if (is.null(elt)) {
            null[[i]] <- TRUE
            next
        }

        if (length(dim(elt)) <= 1) {
            if (is.list(elt) && !is.object(elt)) {
                stop("argument has a list column")
            }
            x[[i]] <- xtfrm(elt)
        } else {
            x[[i]] <- xtfrm.dataset(elt)
        }
    }

    x <- x[!null]
    n <- length(x)
    nrow <- nrow(x)

    if (n == 0) {
        return(seq_len(nrow))
    } else if (n == 1) {
        return(x[[1]])
    }

    u <- unique(x)
    o <- do.call(order, u)

    nu <- nrow(u)
    ru <- integer(nu)
    ru[o] <- seq_len(nu)

    if (nu == nrow)
        ru
    else
        ru[lookup(x, u)]
}
