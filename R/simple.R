
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
    x <- lapply(x, as.simple)
    as.record(x)
}


as.simple.dataset <- function(x)
{
    x <- as.dataset(x)
    n <- nrow(x)
    k <- keys(x)
    x <- as.simple.record(x)
    if (length(x) == 0) {
        x <- as.dataset(matrix(0, n, 0))
    } else {
        x <- as.dataset(x)
    }
    keys(x) <- k
    x
}
