convert <- function(x, type)
{
    UseMethod("convert")
}


convert.default <- function(x, type)
{
    if (is.record(x) || is.record(type)) {
        if (is.dataset(x)) {
            x <- convert.dataset(x, type)
        } else {
            x <- convert.record(x, type)
        }
        return(x)
    }

    r <- length(dim(x))
    if (r == 2) {
        x <- as.dataset(x)
        return(convert.dataset(x, type))
    } else if (r > 2) {
        stop(sprintf("cannot convert rank-%.0f object to another type", r))
    }

    n <- length(x)
    if (is.null(type)) {
        if (n != 0) {
            stop(sprintf("cannot convert length-%.0f object to type NULL", n))
        }
    } else {
        type[seq_len(n)] <- x
    }

    type
}


convert.record <- function(x, type)
{
    x  <- as.simple.record(x)
    nx <- length(x)
    n  <- if (is.record(type)) length(type) else 1

    if (n != nx) {
        stop(sprintf("mismatch: values have %.0f components, type has %.0f",
                     nx, n))
    }

    if (is.record(type)) {
        x <- mapply(convert, x, type, SIMPLIFY = FALSE)
        as.record(x)
    } else {
        y <- type[[i]]
        convert(x[[1]], type)
    }
}


convert.dataset <- function(x, type)
{
    x <- as.dataset(x)
    n <- nrow(x)
    k <- keys(x)
    x <- convert.record(x, type)
    if (length(x) == 0) {
        x <- as.dataset(matrix(0, n, 0))
    } else {
        x <- as.dataset(x)
    }
    keys(x) <- k
    x
}
