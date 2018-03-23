
idproxy <- function(x)
{
    UseMethod("idproxy")
}


idproxy.default <- function(x)
{
    if (is.null(x))
        return(x)

    d <- dim(x)
    r <- length(d)

    if (r <= 1) {
        idproxy.vector(x)
    } else if (r == 2) {
        idproxy.matrix(x)
    } else {
        stop(sprintf("cannot compute idproxy for rank-%.0f objects", r))
    }
}


idproxy.vector <- function(x)
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
        stop(sprintf("cannot compute idproxy for objects of class \"%s\"",
                     paste(cl, collapse = ".")))
    }
}


idproxy.matrix <- function(x)
{
    d <- dim(x)
    if (length(d) != 2)
        stop("argument is not a matrix")

    x <- as.dataset.matrix(x)
    idproxy.dataset(x)
}


idproxy.dataset <- function(x)
{
    x <- as.dataset(x)
    null <- logical(length(x))

    for (i in seq_along(x)) {
        p <- idproxy(x[[i]])
        if (is.null(p)) {
            null[[i]] <- TRUE
        } else {
            x[[i]] <- p
        }
    }

    x[!null]
}
