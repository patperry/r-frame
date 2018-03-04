
as.normal <- function(x)
{
    UseMethod("as.normal")
}


as.normal.default <- function(x)
{
    if (is.null(x))
        return(x)

    d <- dim(x)
    r <- length(d)
    if (r == 2) {
        x <- as.dataset(x)
        return(as.normal(x))
    } else if (r > 2) {
        stop(sprintf("cannot convert rank-%.0f objects to normal", r))
    }

    cl <- oldClass(x)
    if ((!is.null(cl)) && is.numeric(x)) {
        x  <- as.numeric(x)
        cl <- oldClass(x)
    }

    if (!is.null(cl)) {
        stop(sprintf("cannot convert objects of class \"%s\" to normal", cl[[1]]))
    }

    mode <- storage.mode(x)
    if (mode == "character") {
        x <- as_utf8(x, normalize = TRUE)
        x[!nzchar(x)] <- NA
    } else if (mode == "double") {
        x[is.nan(x)] <- NA
    } else if (mode == "integer") {
        # pass
    } else if (mode == "logical") {
        # pass
    } else if (mode == "complex") {
        x <- dataset(re = as.normal(Re(x)), im = as.normal(Im(x)))
    } else if (mode == "list") {
        lengths <- vapply(x, length, 0)
        if (!all(lengths == 1)) {
            stop(sprintf("cannot convert heterogeneous list to normal"))
        }
        x <- do.call(c, x)
        x <- as.normal(x)
    } else if (mode == "raw") {
        x <- as.integer(x)
    } else {
        stop(sprintf("cannot convert objects of mode \"%s\" to normal", mode))
    }

    x
}


as.normal.dataset <- function(x)
{
    x <- as.dataset(x)
    for (i in seq_along(x)) {
        x[[i]] <- as.normal(x[[i]])
    }
    x
}


as.normal.record <- function(x)
{
    x <- as.record(x)
    as.normal.dataset(x)
}


as.normal.factor <- function(x)
{
    x <- as.factor(x)
    levels <- as.normal(levels(x))
    levels[x]
}


as.normal.Date <- function(x)
{
    as.Date(x)
}


as.normal.POSIXt <- function(x)
{
    as.POSIXct(x)
}
