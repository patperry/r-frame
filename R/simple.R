
as.simple <- function(x)
{
    UseMethod("as.simple")
}


as.simple.default <- function(x)
{
    if (is.null(x))
        return(NULL)

    d <- dim(x)
    r <- length(d)
    if (r == 2) {
        x <- as.dataset(x)
        return(as.simple(x))
    } else if (r > 2) {
        stop(sprintf("cannot convert rank-%.0f objects to simple", r))
    }

    cl <- oldClass(x)
    if ((!is.null(cl)) && is.numeric(x)) {
        x  <- as.numeric(x)
        cl <- oldClass(x)
    }

    if (!is.null(cl)) {
        stop(sprintf("cannot convert objects of class \"%s\" to simple", cl[[1]]))
    }

    mode <- storage.mode(x)
    if (mode == "character") {
        x <- as_utf8(x)
        x[!nzchar(x)] <- NA
    } else if (mode == "double") {
        x[is.nan(x)] <- NA
    } else if (mode == "integer") {
        # pass
    } else if (mode == "logical") {
        # pass
    } else if (mode == "complex") {
        x <- dataset(re = as.simple(Re(x)), im = as.simple(Im(x)))
    } else if (mode == "list") {
        lengths <- vapply(x, length, 0)
        if (!all(lengths == 1)) {
            stop(sprintf("cannot convert heterogeneous list to simple"))
        }
        x <- do.call(c, x)
        x <- as.simple(x)
    } else if (mode == "raw") {
        x <- as.integer(x)
    } else {
        stop(sprintf("cannot convert objects of mode \"%s\" to simple", mode))
    }

    x
}


as.simple.dataset <- function(x)
{
    x <- as.dataset(x)
    for (i in seq_along(x)) {
        elt <- x[[i]]
        if (is.null(elt))
            next
        x[[i]] <- elt <- as.simple(elt)
    }
    x
}


as.simple.record <- function(x)
{
    x <- as.record(x)
    as.simple.dataset(x)
}


as.simple.factor <- function(x)
{
    x <- as.factor(x)
    levels <- as.simple(levels(x))
    levels[x]
}


as.simple.Date <- function(x)
{
    as.Date(x)
}


as.simple.POSIXt <- function(x)
{
    as.POSIXct(x)
}
