
as.simple <- function(x)
{
    UseMethod("as.simple")
}


as.simple.dataset <- function(x)
{
    x <- as.dataset(x)
    for (i in seq_along(x)) {
        x[[i]] <- as.simple(x[[i]])
    }
    x
}


as.simple.matrix <- function(x)
{
    x <- as.dataset.matrix(x)
    as.simple.dataset(x)
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
    if (!is.atomic(x)) {
        stop(sprintf("cannot convert non-atomic mode \"%s\" to simple",
                     mode))
    }

    if (mode == "character") {
        x <- as_utf8(x) # TODO: use enc2utf8 instead, check encoding in C code
    } else if (mode == "double") {
        x[x == 0] <- 0      # replace -0 with 0
        x[is.nan(x)] <- NaN # replace all NaN with canonical
    } else if (mode == "complex") {
        x[is.na(x)] <- NA
        re <- as.simple(Re(x))
        im <- as.simple(Im(x))
        x <- complex(real = re, imaginary = im)
    } else {
        # pass
    }

    x
}
