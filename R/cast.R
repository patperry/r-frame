
cast <- function(type, x)
{
    UseMethod("cast")
}


cast.default <- function(type, x)
{
    type <- schema(type)
    if (is.record(type)) {
        cast.record(type, x)
    } else {
        cast.vector(type, x)
    }
}


cast.vector <- function(type, x)
{
    type <- as.vector.type(type)
    x    <- as.vector.value(x)
    n    <- length(x)

    if (is.null(type)) {
        if (n != 0) {
            stop(sprintf("cannot cast from length-%.0f vector to NULL", n))
        }
        return(type)
    } else if (is.atomic(type) && !is.object(type)) {
        mode <- storage.mode(type)
        x    <- as.vector(x, mode)
    }

    type[seq_len(n)] <- x
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
    type <- as.record.type(type)
    if (length(dim(x)) <= 1) {
        x <- as.record(x)
    } else {
        x <- as.dataset(x)
    }

    x  <- as.simple(x)
    nx <- length(x)
    n  <- length(type)

    if (n != nx) {
        stop(sprintf("mismatch: destination has %.0f components, source has %.0f",
                     n, nx))
    }

    names <- names(type)
    if (is.null(names)) {
        names(x) <- NULL
    } else {
        namesx <- names(x)
        if (is.null(namesx)) {
            names(x) <- names
        } else if (!identical(names, namesx)) {
            i <- which(!mapply(identical, names, namesx))[[1]]
            nfmt <- function(nm) if (is.na(nm)) "<NA>"
                                 else paste0('`', nm, '`')
            fmt <- "mismatch: destination component %.0f has name %s, source has name %s"
            stop(sprintf(fmt, i, nfmt(names[[i]]), nfmt(namesx[[i]])))
        }
    }

    for (i in seq_len(n)) {
        x[[i]] <- cast(type[[i]], x[[i]])
    }

    x
}
