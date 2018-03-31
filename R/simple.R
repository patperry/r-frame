
as.simple <- function(x)
{
    UseMethod("as.simple")
}


as.simple.default <- function(x)
{
    r <- length(dim(x))
    if (r == 1) {
        if (is.record(x)) {
            as.simple.record(x)
        } else {
            x
        }
    } else if (r == 2) {
        x <- as.dataset(x)
        as.simple.record(x)
    } else if (r > 2) {
        stop(sprintf("cannot convert rank-%.0f objects to simple", r))
    }

    x
}


as.simple.record <- function(x)
{
    x <- as.record(x)
    for (i in seq_along(x)) {
        x[[i]] <- as.simple(x[[i]])
    }
    x
}


as.simple.dataset <- function(x)
{
    x <- as.dataset(x)
    as.simple.record(x)
}
