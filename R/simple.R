
as.simple <- function(x)
{
    UseMethod("as.simple")
}


as.simple.default <- function(x)
{
    r <- length(dim(x))
    if (r == 2) {
        return(as.simple.matrix(x))
    } else if (r > 2) {
        stop(sprintf("cannot convert rank-%.0f object to simple", 2))
    }

    if (is.object(x)) {
        cl <- class(x)
        stop(sprintf("cannot convert object of class \"%s\" to simple",
                     paste(cl, collapse = ".")))
    }

    mode <- storage.mode(x)
    stop(sprintf("cannot convert non-atomic mode \"%s\" to simple", mode))
}


as.simple.matrix <- function(x)
{
    x <- as.dataset.matrix(x)
    as.simple.dataset(x)
}


as.simple.dataset <- function(x)
{
    x <- as.dataset(x)
    for (i in seq_along(x)) {
        x[[i]] <- as.simple(x[[i]])
    }
    x
}


as.simple.NULL <- function(x)
{
    NULL
}


as.simple.logical <- function(x)
{
    if (!is.logical(x)) {
        x <- as.logical(x)
    }
    x
}


as.simple.raw <- function(x)
{
    if (!is.raw(x)) {
        x <- as.raw(x)
    }
    x
}


as.simple.integer <- function(x)
{
    if (!is.integer(x)) {
        x <- as.integer(x)
    }
    x
}


as.simple.double <- function(x)
{
    if (!is.double(x)) {
        x <- as.double(x)
    }
    .Call(rframe_as_simple_double, x)
}


as.simple.numeric <- function(x)
{
    as.simple.double(x)
}


as.simple.complex <- function(x)
{
    if (!is.complex(x)) {
        x <- as.complex(x)
    }

    at <- attributes(x)
    re <- as.simple.double(Re(x))
    im <- as.simple.double(Im(x))

    na <- (is.na(re) & !is.nan(re)) | (is.na(im) & !is.nan(im))
    re[na] <- NA
    im[na] <- NA

    x <- complex(real = re, imaginary = im)
    attributes(x) <- at

    x
}


as.simple.character <- function(x)
{
    if (!is.character(x)) {
        x <- as.character(x)
    }
    x <- enc2utf8(x)
    x
}


as.simple.factor <- function(x)
{
    if (!is.factor(x)) {
        x <- as.factor(x)
    }
    x
}


as.simple.POSIXt <- function(x)
{
    if (!inherits(x, "POSIXct")) {
        x <- as.POSIXct(x)
    }
    as.simple.double(x)
}
