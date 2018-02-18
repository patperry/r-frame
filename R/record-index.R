
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
    else entry
}



`[[<-.record` <- function(x, i, value)
{
    # TODO: implement in C

    if (missing(i))
        stop("missing index")

    n <- length(i)
    i1 <- arg_record_index1(n, i)

    if (n == 1) {
        x[i1] <- value
    } else {
        x[[i1]][[ i[-1] ]] <- value
    }

    x
}


`[.record` <- function(x, i)
{
    i <- arg_record_subset(x, i)
    if (is.null(i)) {
        x
    } else {
        y <- .subset(x, i)

        names <- names(i)
        if (is.null(names)) {
            names <- names(y)
        }

        if (anyNA(names)) {
            names[is.na(names)] <- ""
        }

        names(y) <- names
        class(y) <- "record"
        y
    }
}


`[<-.record` <- function(x, i, value)
{
    i <- arg_record_subset(x, i)

    if (is.null(value))
        record_delete(x, i)
    else record_replace(x, i, value)
}


record_delete <- function(x, i)
{
    if (is.null(i))
        return(record_delete_all(x))

    if (is.character(i))
        i <- match(i, names(x))

    class(x) <- NULL
    x[i] <- NULL
    class(x) <- "record"

    x
}


record_delete_all <- function(x)
{
    class(x) <- NULL
    x[] <- NULL
    class(x) <- "record"
    x
}


record_replace <- function(x, i, value, call = sys.call(-1))
{
    if (is.null(i))
        record_replace_all(x, value, call)
    else if (is.numeric(i))
        if (any(i < 0L))
            record_replace_except(x, i, value, call)
        else
            record_replace_index(x, length(x), i[i > 0L], value, call)
    else if (is.character(i))
        record_replace_names(x, i, value, call)
    else record_replace_index_unsafe(x, which(i), value, call)
}


record_replace_all <- function(x, value, call)
{
    n <- length(x)
    nv <- length(value)

    if (nv == n) {
        if (is.object(value))
            value <- as.list(value)
    } else if (nv == 1) {
        if (is.object(value))
            value <- value[[1]]
        value <- rep_len(value, n)
    } else {
        fmt <- "mismatch: replacement length is %.0f, selection length is %.0f"
        stop(simpleError(sprintf(fmt, nv, n), call))
    }

    class(x) <- NULL
    x[] <- value
    class(x) <- "record"

    x
}


record_replace_except <- function(x, i, value, call)
{
    if (anyNA(i))
        i[is.na(i)] <- NA

    if (!is.integer(i))
        i <- trunc(i)

    if (any(i > 0L)) {
        fmt <- "numeric index contains both positive and negative values"
        stop(simpleError(fmt, call))
    }

    n <- length(x)
    record_replace_index(x, n, seq_len(n)[i], value, call)
}


record_replace_index <- function(x, n, i, value, call)
{
    # positive integer indices, some above length(x)
    new <- which(i > n)
    nnew <- length(new)

    if (nnew > 0L) {
        n <- n + nnew
        length(x) <- n
    }

    record_replace_index_unsafe(x, i, value, call)
}


record_replace_index_unsafe <- function(x, i, value, call)
{
    # positive integer indices, all within bounds;

    ni <- length(i)
    nv <- length(value)
    if (ni != nv && nv != 1L) {
        fmt <- "mismatch: selection size is %.0f, replacement size is %.0f"
        stop(simpleError(sprintf(fmt, ni, nv), call))
    }

    class(x) <- NULL
    x[i] <- value
    names <- names(i)
    if (!is.null(names)) {
        names(x)[i] <- names
    }
    oldClass(x) <- "record"

    x
}


record_replace_names <- function(x, i, value, call)
{
    n <- length(x)
    names <- names(x)

    index <- match(i, names, 0L)
    new <- which(!index)
    nnew <- length(new)

    if (nnew > 0L) {
        n1 <- n + nnew
        inew <- (n + 1L):n1
        index[new] <- inew

        if (is.null(names))
            names <- character(n1)

        names(x) <- NULL
        length(x) <- n1

        oldClass(x) <- NULL
        names[inew] <- arg_record_names(-1, i[new], "replacement names")
        names(x) <- names
    }

    record_replace_index_unsafe(x, index, value, call)
}
