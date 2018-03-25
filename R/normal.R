
as.normal <- function(x)
{
    UseMethod("as.normal")
}

is.normal <- function(x)
{
    inherits(x, "normal") || is.null(x) || is.normal.dataset(x)
}

is.normal.dataset <- function(x)
{
    is.dataset(x) && all(vapply(x, is.normal, FALSE))
}

as.normal.normal <- function(x)
{
    x
}

as.normal.default <- function(x)
{
    if (is.null(x))
        return(x)

    d <- dim(x)
    r <- length(d)
    if (r == 2) {
        x <- as.dataset(x)
        return(as.normal(x))
    } else if (r > 2) {
        stop(sprintf("cannot convert rank-%.0f objects to normal", r))
    }

    cl <- oldClass(x)
    if ((!is.null(cl)) && is.numeric(x)) {
        x  <- as.numeric(x)
        cl <- oldClass(x)
    }

    if (!is.null(cl)) {
        stop(sprintf("cannot convert objects of class \"%s\" to normal", cl[[1]]))
    }

    mode <- storage.mode(x)
    if (mode == "character") {
        x <- as_utf8(x)
    } else if (mode == "double") {
        x
    } else if (mode == "integer") {
        # pass
    } else if (mode == "logical") {
        # pass
    } else if (mode == "complex") {
        x[is.na(x)] <- NA
    } else if (mode == "raw") {
        # pass
    } else {
        stop(sprintf("cannot convert objects of mode \"%s\" to normal", mode))
    }

    class(x) <- c("normal", class(x))
    x
}


as.normal.dataset <- function(x)
{
    if (is.normal.dataset(x))
        return(x)

    x <- as.dataset(x)
    k <- keys(x)
    x <- as.record(lapply(x, as.normal))
    x <- as.dataset(x)
    keys(x) <- k
    x
}


as.normal.Date <- function(x)
{
    class(x) <- c("normal", "Date")
    x
}


as.normal.POSIXct <- function(x)
{
    class(x) <- c("normal", "POSIXct", "POSIXt")
    x
}


as.normal.POSIXlt <- function(x)
{
    x <- as.POSIXct(x)
    as.normal(x)
}


as.normal.factor <- function(x)
{
    levels(x) <- as.normal(levels(x))
    cl <- if (is.ordered(x))
              c("normal", "ordered", "factor")
          else
              c("normal", "factor")
    class(x) <- cl
    x
}


un.normal <- function(x)
{
    cl <- oldClass(x)
    k  <- match("normal", cl, 0L)
    if (k > 0L) {
        m  <- length(cl)
        class(x) <- cl[(k + 1L):m]
    }
    x
}


`[[.normal` <- function(x, i)
{

    x <- NextMethod("[[")
    un.normal(x)
}


`[.normal` <- function(x, i)
{
    cl <- oldClass(x)
    x <- NextMethod("[")
    class(x) <- cl
    x
}


`[[<-.normal` <- function(x, i, value)
{
    x <- NextMethod("[[<-")
    un.normal(x)
}


`[<-.normal` <- function(x, i, value)
{
    x <- NextMethod("[<-")
    un.normal(x)
}


`levels<-.normal` <- function(x, value)
{
    x <- NextMethod("levels<-")
    un.normal(x)
}
