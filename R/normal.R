
as.normal <- function(x)
{
    UseMethod("as.normal")
}


as.normal.record <- function(x)
{
    x <- as.record(x)
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


as.normal.matrix <- function(x)
{
    x <- as.dataset.matrix(x)
    as.normal.dataset(x)
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
    if (!is.atomic(x)) {
        stop(sprintf("cannot convert non-atomic mode \"%s\" to normal",
                     mode))
    }

    if (mode == "character") {
        x <- as_utf8(x) # TODO: use enc2utf8 instead, check encoding in C code
    } else if (mode == "double") {
        x[x == 0] <- 0      # replace -0 with 0
        x[is.nan(x)] <- NaN # replace all NaN with canonical
    } else if (mode == "complex") {
        x[is.na(x)] <- NA
        re <- as.normal(Re(x))
        im <- as.normal(Im(x))
        x <- complex(real = re, imaginary = im)
    } else {
        # pass
    }

    x
}
