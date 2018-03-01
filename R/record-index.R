
`$.record` <- function(x, name)
{
    x[[name]]
}


`$<-.record` <- function(x, name, value)
{
    x[[name]] <- value
    x
}


`[[.record` <- function(x, i, exact = TRUE)
{
    # TODO: implement in C

    if (!identical(exact, TRUE))
        warning("'exact' argument is ignored")

    if (missing(i))
        stop("missing index")

    n <- length(i)
    i1 <- arg_record_index1(n, i)

    x1 <- x[i1]
    entry <- .subset2(x1, 1)

    if (n > 1)
        entry[[ i[-1] ]]
    else
        entry
}



`[[<-.record` <- function(x, i, value)
{
    # TODO: implement in C

    if (missing(i))
        stop("missing index")

    n <- length(i)
    i1 <- arg_record_index1(n, i)

    if (n == 1) {
        if (!is.null(value)) {
            value <- list(value)
        }
        x[i1] <- value
    } else {
        entry <- x[[i1]]
        entry[[ i[-1] ]] <- value
        x[[i1]] <- entry
    }

    x
}


`[.record` <- function(x, i)
{
    i <- arg_record_subset(x, i, TRUE)
    if (is.null(i)) {
        x
    } else {
        y <- .subset(x, i)

        names <- names(i)
        if (!is.null(names)) {
            empty <- is.na(names) | !nzchar(names)
            names(y)[!empty] <- names[!empty]
        }

        as.record(y)
    }
}


`[<-.record` <- function(x, i, value)
{
    i <- arg_record_subset(x, i, FALSE)

    if (is.null(value))
        record_delete(x, i)
    else
        record_replace(x, i, value)
}


record_delete <- function(x, i)
{
    class(x) <- NULL
    if (is.null(i)) {
        x[] <- NULL
    } else {
        x[i] <- NULL
    }
    as.record(x)
}


record_replace <- function(x, i, value, call = sys.call(-1))
{
    if (is.null(i))
        i <- seq_along(x)

    # positive integer indices, all within bounds;
    ni <- length(i)
    nv <- length(value)
    if (ni != nv && nv != 1L) {
        fmt <- "mismatch: selection length is %.0f, replacement length is %.0f"
        stop(simpleError(sprintf(fmt, ni, nv), call))
    }

    zero <- (i == 0)
    if (any(zero)) {
        i <- i[!zero]
        value <- value[!zero]
    }

    class(x) <- NULL
    x[i] <- value

    names <- names(i)
    if (!is.null(names)) {
        empty <- is.na(names) | !nzchar(names)
        names(x)[i[!empty]] <- names[!empty]
    }

    as.record(x)
}
