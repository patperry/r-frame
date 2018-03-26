cast_as <- function(type, x)
{
    UseMethod("cast_as")
}


cast_as.default <- function(type, x)
{
    r <- length(dim(x))
    if (r == 2) {
        x <- as.simple.dataset(x)
    } else if (r > 2) {
        stop(sprintf("cannot cast rank-%.0f object", r))
    }

    if (is.record(x)) {
        nx <- length(x)
        if (nx != 1) {
            stop(sprintf("mismatch: type has 1 components, values have %.0f",
                         nx))
        }
        return(cast_as(type, x[[1]]))
    }

    n <- length(x)
    if (is.null(type)) {
        if (n != 0) {
            stop(sprintf("cannot cast length-%.0f object to NULL", n))
        }
        return(NULL)
    }

    type[seq_len(n)] <- x
    type
}


cast_as.record <- function(type, x)
{
    x  <- as.simple.record(x)
    nx <- length(x)
    n  <- length(type)

    if (n != nx) {
        stop(sprintf("mismatch: type has %.0f components, values have %.0f",
                     n, nx))
    }

    for (i in seq_len(n)) {
        x[[i]] <- cast_as(type[[i]], x[[i]])
    }

    x
}
