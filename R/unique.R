

unique.dataset <- function(x, incomparables = FALSE, ...)
{
    x <- as.dataset(x)
    p <- idproxy(x)
    p <- as.normal(p)
    u <- .Call(rframe_unique, p)
    x[u$types, ]
}


duplicated.dataset <- function(x, incomparables = FALSE, ...)
{
    x <- as.dataset(x)
    x <- idproxy(x)
    x <- as.normal(x)
    u <- .Call(rframe_unique, x)
    duplicated(u$group)
}


anyDuplicated.dataset <- function(x, incomparables = FALSE, ...)
{
    x <- as.dataset(x)
    x <- idproxy(x)
    x <- as.normal(x)
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
