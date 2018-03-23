
proxy <- function(x)
{
    UseMethod("proxy")
}


proxy.default <- function(x)
{
    if (is.null(x))
        return(x)

    d <- dim(x)
    r <- length(d)

    if (r <= 1) {
        proxy.vector(x)
    } else if (r == 2) {
        proxy.matrix(x)
    } else {
        stop(sprintf("cannot compute proxy for rank-%.0f objects", r))
    }
}


proxy.vector <- function(x)
{
    d <- dim(x)
    if (length(d) > 1)
        stop("argument is not a vector")

    if (is.object(x)) {
        xtfrm(x)
    } else if (is.atomic(x)) {
        x
    } else {
        cl <- class(x)
        stop(sprintf("cannot compute proxy for objects of class \"%s\"",
                     paste(cl, collapse = ".")))
    }
}


proxy.matrix <- function(x)
{
    d <- dim(x)
    if (length(d) != 2)
        stop("argument is not a matrix")

    x <- as.dataset.matrix(x)
    proxy.dataset(x)
}


proxy.dataset <- function(x)
{
    x <- as.dataset(x)
    null <- logical(length(x))

    for (i in seq_along(x)) {
        p <- proxy(x[[i]])
        if (is.null(p)) {
            null[[i]] <- TRUE
        } else {
            x[[i]] <- p
        }
    }

    x[!null]
}
