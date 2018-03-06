
prototype <- function(x)
{
    x <- as.dataset(x)
    for (i in seq_along(x)) {
        xi <- x[[i]]
        if (length(dim(xi)) >= 2) {
            x[[i]] <- prototype(xi)
        } else if (is.object(xi) && !is.normal(xi)) {
            x[[i]] <- xtfrm(xi)
        } else if (is.complex(xi) || is.raw(xi)) {
            x[[i]] <- xtfrm(xi)
        } else if (is.character(xi)) {
            x[[i]] <- as_utf8(xi)
        } else if (is.list(xi)) {
            stop("cannot use 'unique' or 'duplicated' with \"list\" column")
        }
    }
    x
}


unique.dataset <- function(x, incomparables = FALSE, ...)
{
    p <- prototype(x)
    u <- .Call(rframe_unique, p)
    x[u$types, ]
}


duplicated.dataset <- function(x, incomparables = FALSE, ...)
{
    x <- prototype(x)
    u <- .Call(rframe_unique, x)
    duplicated(u$group)
}


anyDuplicated.dataset <- function(x, incomparables = FALSE, ...)
{
    x <- prototype(x)
    u <- .Call(rframe_unique, x)

    if (length(u$types) < length(u$group))
        anyDuplicated(u$group)
    else
        0L
}


unique.keyset <- function(x, incomparables = FALSE, ...)
{
    as.keyset(x)
}


duplicated.keyset <- function(x, incomparables = FALSE, ...)
{
    x <- as.keyset(x)
    logical(nrow(x))
}


anyDuplicated.keyset <- function(x, incomparables = FALSE, ...)
{
    x <- as.keyset(x)
    0L
}
