
as.simple <- function(x)
{
    UseMethod("as.simple")
}


as.simple.default <- function(x)
{
    if (is.null(x))
        return(NULL)

    d <- dim(x)
    r <- length(d)
    if (r == 2) {
        return(as.simple.dataset(x))
    } else if (r > 2) {
        stop(sprintf("cannot convert rank-%.0f objects to simple", r))
    }

    if (is.numeric(x)) {
        x <- as.numeric(x)
    }

    cl <- oldClass(x)
    if (!is.null(cl)) {
        stop(sprintf("cannot convert objects of class \"%s\" to simple", cl[[1]]))
    }

    mode <- storage.mode(x)
    if (mode == "character") {
        as.simple.character(x)
    } else if (mode == "double") {
        as.simple.double(x)
    } else if (mode == "integer" || mode == "logical" || mode == "NULL") {
        # pass
    } else if (mode == "complex") {
        as.simple.complex(x)
    } else if (mode == "list") {
        as.simple.list(x)
    } else if (mode == "raw") {
        as.simple.integer(x)
    } else {
        stop(sprintf("cannot convert objects of mode \"%s\" to simple", mode))
    }
}


as.simple.dataset <- function(x)
{
    x <- as.dataset(x)
    for (i in seq_along(x)) {
        elt <- x[[i]]
        if (is.null(elt))
            next
        x[[i]] <- elt <- as.simple(elt)
    }
    x
}


as.simple.record <- function(x)
{
    x <- as.record(x)
    as.simple.dataset(x)
}


as.simple.logical <- function(x)
{
    as.logical(x)
}


as.simple.integer <- function(x)
{
    as.integer(x)
}

as.simple.double <- function(x)
{
    x <- as.double(x)
    x[is.nan(x)] <- NA
    x
}

as.simple.character <- function(x)
{
    x <- as.character(x)
    x <- as_utf8(x)
    x[!nzchar(x)] <- NA
    x
}


as.simple.complex <- function(x)
{
    x <- as.complex(x)
    dataset(re = as.simple(Re(x)),
            im = as.simple(Im(x)))
}


as.simple.factor <- function(x)
{
    x <- as.factor(x)
    levels <- as.simple(levels(x))
    levels[x]
}


as.simple.list <- function(x)
{
    x <- as.list(x)
    lengths <- vapply(x, length, 0)
    if (!all(lengths == 1L)) {
        stop(sprintf("cannot convert heterogeneous list to simple vector"))
    }

    x <- do.call(c, x)
    as.simple(x)
}

# POSIXct: internally stored as numeric, number of seconds since 1970-01-01 UTC
#          "tzone" attribute stores time zone
#
# as.POSIXct.POSIXct(, tz) ignores tz
# as.POSIXlt.POSIXct(, tz) keeps time, changes to new zone
# as.Date(, tz) keeps time, changes to new zone, then gets date
#
# POSIXlt: list with year/month/day/etc components, along with time zone
#          also has tzone attribute with zone
#
# as.POSIXct.POSIXlt(, tz) changes to new time; (e.g. 6pm EST -> 6pm PST)
# as.POSIXlt.POSIXlt(, tz) ignores tz
# as.Date(, tz) ignores tz, uses y/m/d
#
# Date: internally stored as numeric, number of days since 1970-01-01 UTC

get_tzone <- function(x, default = "UTC")
{
    tz <- attr(x, "tzone")[[1L]]
    if (is.null(tz)) {
        tz <- default[[1L]]
    }
    tz
}

as.simple.Date <- function(x)
{
    tz <- get_tzone(x)
    x <- as.Date(x, tz = tz, origin = "1970-01-01")
    structure(as.numeric(x), class = "Date")
}

as.simple.POSIXt <- function(x, tz = NULL)
{
    tz0 <- get_tzone(x, tz)
    x <- as.POSIXct(x, tz0, origin = "1970-01-01")
    tzone <- if (is.null(tz)) attr(x, "tzone") else tz
    structure(as.numeric(x), class = c("POSIXct", "POSIXt"), tzone = tzone)
}
