
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


to_vector_type <- function(type)
{
    if (!(length(x) == 0 && length(dim(x)) <= 1))
        stop("argument is not a vector type")
    type
}


from_vector <- function(x)
{
    r <- length(dim(x))
    if (r == 2) {
        x <- as.simple.dataset(x)
    } else if (r > 2) {
        stop(sprintf("cannot cast from rank-%.0f object", r))
    }

    if (is.record(x)) {
        nx <- length(x)
        if (nx != 1) {
            stop(sprintf("mismatch: type has 1 components, object has %.0f",
                         nx))
        }
        return(from_vector(x[[1]]))
    }

    x
}


cast_as.NULL <- function(type, x)
{
    type <- to_vector_type(type)
    x    <- from_vector(x)
    n <- length(x)
    if (n != 0)
        stop(sprintf("cannot cast from length-.%0f object to NULL", n))
    NULL
}


cast_as.logical <- function(type, x)
{
    type <- to_vector_type(type)
    x    <- from_vector(x)
    as.logical(x)
}


cast_as.integer <- function(type, x)
{
    type <- to_vector_type(type)
    x    <- from_vector(x)
    as.integer(x)
}


cast_as.double <- function(type, x)
{
    type <- to_vector_type(type)
    x    <- from_vector(x)
    as.double(x)
}


cast_as.complex <- function(type, x)
{
    type <- to_vector_type(type)
    x    <- from_vector(x)
    as.complex(x)
}


cast_as.character <- function(type, x)
{
    type <- to_vector_type(type)
    x    <- from_vector(x)
    as.character(x)
}


cast_as.factor <- function(type, x)
{
    type <- to_vector_type(type)
    x    <- from_vector(x)

    n <- length(x)
    type[seq_len(n)] <- x
    type
}


cast_as.Date <- function(type, x)
{
    type <- to_vector_type(type)
    x    <- from_vector(x)

    tz <- get_tzone(x)
    x  <- as.Date(x, tz = tz, origin = "1970-01-01")
    structure(as.numeric(x), class = "Date")
}


cast_as.POSIXct <- function(type, x)
{
    type <- to_vector_type(type)
    x    <- from_vector(x)

    tz  <- get_tzone(type)
    tz0 <- get_tzone(x, tz)
    x <- as.POSIXct(x, tz0, origin = "1970-01-01")
    structure(as.numeric(x), class = c("POSIXct", "POSIXt"), tzone = tz)
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
        stop(sprintf("mismatch: type has %.0f components, values have %.0f",
                     n, nx))
    }

    for (i in seq_len(n)) {
        x[[i]] <- cast_as(type[[i]], x[[i]])
    }

    x
}
