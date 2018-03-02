
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
    if (r <= 1) {
        as.simple.vector(x)
    } else if (r == 2) {
        x <- as.dataset(x)
        as.simple(x)
    } else {
        stop(sprintf("cannot convert rank-%.0f objects to simple", r))
    }
}


as.simple.vector <- function(x)
{
    d <- dim(x)
    if (length(d) > 1)
        stop("argument is not a vector")

    if (is.numeric(x)) {
        x <- as.numeric(x)
    }

    cl <- oldClass(x)
    if (!is.null(cl)) {
        stop(sprintf("cannot convert objects of class \"%s\" to simple", cl[[1]]))
    }

    mode <- storage.mode(x)
    if (mode == "character") {
        x <- as_utf8(x)
        x[!nzchar(x)] <- NA
        x
    } else if (mode == "double") {
        x[is.nan(x)] <- NA
        x
    } else if (mode %in% c("integer", "logical")) {
        # pass
    } else if (mode == "NULL") {
        NULL
    } else if (mode == "complex") {
        dataset(re = as.simple(Re(x)), im = as.simple(Im(x)))
    } else if (mode == "list") {
        lengths <- vapply(x, length, 0)
        if (all(lengths == 1L)) {
            x <- do.call(c, x)
            x <- as.simple(x)
        } else {
            stop(sprintf("cannot convert heterogeneous list to simple vector"))
        }
    } else if (mode == "raw") {
        as.integer(x)
    } else {
        stop(sprintf("cannot convert objects of mode \"%s\" to simple", mode))
    }
}


as.simple.dataset <- function(x)
{
    for (i in seq_along(x)) {
        elt <- x[[i]]
        if (is.null(elt))
            next
        x[[i]] <- elt <- as.simple(elt)
    }
    x
}


as.simple.Date <- function(x)
{
    x
}


as.simple.POSIXlt <- function(x)
{
    as.POSIXct(x)
}


as.simple.POSIXct <- function(x)
{
    x
}


as.simple.factor <- function(x)
{
    levels <- as.simple(levels(x))
    x <- unclass(x)
    levels[x]
}
