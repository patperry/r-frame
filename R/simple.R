
# a 'simple' object is either vector-like or a record of simple objects

as.simple <- function(x)
{
    if (is.record(x)) {
        for (i in seq_along(x)) {
            x[[i]] <- as.simple(x[[i]])
        }
    } else {
        r <- length(dim(x))
        if (r <= 1) {
            # pass
        } else {
            x <- as.dataset(x)
            x <- as.simple(x)
        }
    }

    x
}
