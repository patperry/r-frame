
cast_as <- function(type, x)
{
    UseMethod("cast_as")
}


cast_as.default <- function(type, x)
{
    cl <- oldClass(type)
    if (!is.null(cl)) {
        stop(sprintf("cannot cast to type \"%s\"", paste(cl, collapse = ".")))
    }

    mode <- storage.mode(type)
    if (mode == "NULL")
        cast_as.NULL(type, x)
    else if (mode == "logical")
        cast_as.logical(type, x)
    else if (mode == "raw")
        cast_as.raw(type, x)
    else if (mode == "integer")
        cast_as.integer(type, x)
    else if (mode == "double")
        cast_as.double(type, x)
    else if (mode == "complex")
        cast_as.complex(type, x)
    else if (mode == "character")
        cast_as.character(type, x)
    else
        stop(sprintf("cannot cast from object of mode \"%s\"", mode))
}


cast_as.NULL <- function(type, x)
{
    type <- as.vector.type(type)
    x    <- as.vector.value(x)
    n <- length(x)
    if (n != 0)
        stop(sprintf("cannot cast from length-.%0f object to NULL", n))
    type
}


cast_as.logical <- function(type, x)
{
    type <- as.vector.type(type)
    x    <- as.vector.value(x)

    x <- as.logical(x)
    type[seq_along(x)] <- x
    type
}


cast_as.raw <- function(type, x)
{
    type <- as.vector.type(type)
    x    <- as.vector.value(x)

    x <- as.raw(x)
    type[seq_along(x)] <- x
    type
}


cast_as.integer <- function(type, x)
{
    type <- as.vector.type(type)
    x    <- as.vector.value(x)

    x <- as.integer(x)
    type[seq_along(x)] <- x
    type
}


cast_as.double <- function(type, x)
{
    type <- as.vector.type(type)
    x    <- as.vector.value(x)

    x <- as.double(x)
    type[seq_along(x)] <- x
    type
}


cast_as.complex <- function(type, x)
{
    type <- as.vector.type(type)
    x    <- as.vector.value(x)

    x <- as.complex(x)
    type[seq_along(x)] <- x
    type
}


cast_as.character <- function(type, x)
{
    type <- as.vector.type(type)
    x    <- as.vector.value(x)

    x <- as.character(x)
    type[seq_along(x)] <- x
    type
}


cast_as.factor <- function(type, x)
{
    cast_as.vector(type, x)
}


cast_as.vector <- function(type, x)
{
    type <- as.vector.type(type)
    x    <- as.vector.value(x)

    type[seq_along(x)] <- x
    type
}


cast_as.Date <- function(type, x)
{
    type <- as.vector.type(type)
    x    <- as.vector.value(x)

    tz <- get_tzone(x)
    x  <- as.Date(x, tz = tz, origin = "1970-01-01")

    type[seq_along(x)] <- x
    type
}


cast_as.POSIXct <- function(type, x)
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


cast_as.record <- function(type, x)
{
    x  <- as.simple.record(x)
    nx <- length(x)
    n  <- length(type)

    if (n != nx) {
        stop(sprintf("mismatch: type has %.0f components, value has %.0f",
                     n, nx))
    }

    for (i in seq_len(n)) {
        x[[i]] <- cast_as(type[[i]], x[[i]])
    }

    x
}
