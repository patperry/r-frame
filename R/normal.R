
as.normal <- function(x)
{
    UseMethod("as.normal")
}


as.normal.default <- function(x)
{
    r <- length(dim(x))
    if (r == 2) {
        return(as.normal.matrix(x))
    } else if (r > 2) {
        stop(sprintf("cannot convert rank-%.0f object to normal", 2))
    }

    if (is.object(x)) {
        cl <- class(x)
        stop(sprintf("cannot convert object of class \"%s\" to normal",
                     paste(cl, collapse = ".")))
    }

    mode <- storage.mode(x)
    stop(sprintf("cannot convert non-atomic mode \"%s\" to normal", mode))
}


as.normal.matrix <- function(x)
{
    x <- as.dataset.matrix(x)
    as.normal.dataset(x)
}


as.normal.dataset <- function(x)
{
    x <- as.dataset(x)
    for (i in seq_along(x)) {
        x[[i]] <- as.normal(x[[i]])
    }
    x
}


as.normal.NULL <- function(x)
{
    NULL
}


as.normal.logical <- function(x)
{
    if (!is.logical(x)) {
        x <- as.logical(x)
    }
    x
}


as.normal.raw <- function(x)
{
    if (!is.raw(x)) {
        x <- as.raw(x)
    }
    x
}


as.normal.integer <- function(x)
{
    if (!is.integer(x)) {
        x <- as.integer(x)
    }
    x
}


as.normal.double <- function(x)
{
    if (!is.double(x)) {
        x <- as.double(x)
    }
    x[x == 0] <- 0      # replace -0 with 0
    x[is.nan(x)] <- NaN # replace all NaN with canonical
    x
}


as.normal.numeric <- function(x)
{
    as.normal.double(x)
}


as.normal.complex <- function(x)
{
    if (!is.complex(x)) {
        x <- as.complex(x)
    }
    at <- attributes(x)
    x[is.na(x)] <- NA
    re <- as.normal(Re(x))
    im <- as.normal(Im(x))
    x <- complex(real = re, imaginary = im)
    attributes(x) <- at
    x
}


as.normal.character <- function(x)
{
    if (!is.character(x)) {
        x <- as.character(x)
    }
    x <- enc2utf8(x)
    x
}
