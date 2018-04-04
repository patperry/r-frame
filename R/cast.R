
cast <- function(type, x)
{
    UseMethod("cast")
}


cast.default <- function(type, x)
{
    cl <- oldClass(type)
    if (!is.null(cl)) {
        stop(sprintf("cannot cast to type \"%s\"", paste(cl, collapse = ".")))
    }

    mode <- storage.mode(type)
    if (mode == "NULL")
        cast.NULL(type, x)
    else if (mode == "logical")
        cast.logical(type, x)
    else if (mode == "raw")
        cast.raw(type, x)
    else if (mode == "integer")
        cast.integer(type, x)
    else if (mode == "double")
        cast.double(type, x)
    else if (mode == "complex")
        cast.complex(type, x)
    else if (mode == "character")
        cast.character(type, x)
    else
        stop(sprintf("cannot cast from object of mode \"%s\"", mode))
}


cast.NULL <- function(type, x)
{
    type <- as.vector.type(type)
    x    <- as.vector.value(x)
    n <- length(x)
    if (n != 0)
        stop(sprintf("cannot cast from length-%.0f object to NULL", n))
    type
}


cast.logical <- function(type, x)
{
    type <- as.vector.type(type)
    x    <- as.vector.value(x)

    x <- as.logical(x)
    type[seq_along(x)] <- x
    type
}


cast.raw <- function(type, x)
{
    type <- as.vector.type(type)
    x    <- as.vector.value(x)

    x <- as.raw(x)
    type[seq_along(x)] <- x
    type
}


cast.integer <- function(type, x)
{
    type <- as.vector.type(type)
    x    <- as.vector.value(x)

    x <- as.integer(x)
    type[seq_along(x)] <- x
    type
}


cast.double <-
cast.numeric <- function(type, x)
{
    type <- as.vector.type(type)
    x    <- as.vector.value(x)

    x <- as.double(x)
    type[seq_along(x)] <- x
    type
}


cast.complex <- function(type, x)
{
    type <- as.vector.type(type)
    x    <- as.vector.value(x)

    x <- as.complex(x)
    type[seq_along(x)] <- x
    type
}


cast.character <- function(type, x)
{
    type <- as.vector.type(type)
    x    <- as.vector.value(x)

    x <- as.character(x)
    type[seq_along(x)] <- x
    type
}


cast.factor <- function(type, x)
{
    cast.vector(type, x)
}


cast.vector <- function(type, x)
{
    type <- as.vector.type(type)
    x    <- as.vector.value(x)

    type[seq_along(x)] <- x
    type
}


cast.Date <- function(type, x)
{
    type <- as.vector.type(type)
    x    <- as.vector.value(x)

    tz <- get_tzone(x)
    x  <- as.Date(x, tz = tz, origin = "1970-01-01")

    type[seq_along(x)] <- x
    type
}


cast.POSIXct <- function(type, x)
{
    type <- as.vector.type(type)
    x    <- as.vector.value(x)

    tz  <- get_tzone(type)
    tz0 <- get_tzone(x, tz)
    x <- as.POSIXct(x, tz0, origin = "1970-01-01")
    x <- structure(as.numeric(x), class = c("POSIXct", "POSIXt"), tzone = tz)

    type[seq_along(x)] <- x
    type
}


get_tzone <- function(x, default = "UTC")
{
    tz <- attr(x, "tzone")[[1L]]
    if (is.null(tz)) {
        tz <- default[[1L]]
    }
    tz
}


cast.record <- function(type, x)
{
    x  <- as.record(x)
    x  <- as.simple(x)
    nx <- length(x)
    n  <- length(type)

    if (n != nx) {
        stop(sprintf("mismatch: type has %.0f components, value has %.0f",
                     n, nx))
    }

    names <- names(type)
    if (!is.null(names)) {
        namesx <- names(x)
        if (!is.null(namesx) && !identical(names, namesx)) {
            i <- which(!mapply(identical, names, namex))[[1]]
            nfmt <- function(nm) if (is.na(nm)) "<NA>"
                                 else paste0('`', nm, '`')
            fmt <- "mismatch: type name %.0f is %s, value name is %s"
            stop(sprintf(fmt, i, nfmt(names[[i]]), nfmt(namesx[[i]])))
        }
    }

    for (i in seq_len(n)) {
        x[[i]] <- cast(type[[i]], x[[i]])
    }

    x
}
