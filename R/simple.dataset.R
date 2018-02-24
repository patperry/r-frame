as.simple.dataset <- function(x)
{
    UseMethod("as.simple.dataset")
}


as.simple.dataset.default <- function(x)
{
    x <- as.dataset(x)
    x
}


is.simple.dataset <- function(x)
{
    if (!is.dataset(x)) {
        return(FALSE)
    }

    if (length(x) > .Machine$integer.max) {
        return(FALSE)
    }

    for (i in seq_along(x)) {
        if (!is_simple_vector(x[[i]])) {
            return(FALSE)
        }
    }

    TRUE
}
