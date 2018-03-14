
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
        n1 <- length(x)
        i1 <- arg_subset(i1, n1, names(x), FALSE)

        class(x) <- NULL
        x[[i1]] <- value

        if (!is.null(value)) {
            nm <- names(i1)
            if (!is.null(nm)) {
                names(x)[[i1]] <- nm
            } else {
                n2 <- length(x)
                if (n1 < n2) {
                    names(x)[(n1 + 1L):n2] <- NA_character_
                }
            }
        }

        class(x) <- "record"
    } else {
        entry <- x[[i1]]
        entry[[ i[-1] ]] <- value
        x[[i1]] <- entry
    }

    x
}


`[.record` <- function(x, i)
{
    i <- arg_subset(i, length(x), names(x), TRUE)
    if (is.null(i)) {
        names <- names(x)
        attributes(x) <- NULL
        names(x) <- names
    } else {
        x <- .subset(x, i)

        names <- names(i)
        if (!is.null(names)) {
            empty <- is.na(names) | !nzchar(names)
            names(x)[!empty] <- names[!empty]
        }

    }
    class(x) <- "record"
    x
}


`[<-.record` <- function(x, i, value)
{
    i <- arg_subset(i, length(x), names(x), FALSE)
    if (is.null(i))
        i <- seq_along(x)

    if (is.null(value)) {
        x <- .subset(x, -i)
        class(x) <- "record"
        return(x)
    }

    ni <- length(i)
    nv <- length(value)

    if (ni != nv && nv != 1L) {
        fmt <- "mismatch: selection length is %.0f, replacement length is %.0f"
        stop(sprintf(fmt, ni, nv))
    }

    zero <- (i == 0)
    if (any(zero)) {
        i <- i[!zero]
        value <- value[!zero]
    }

    class(x) <- NULL

    nx1 <- length(x)
    x[i] <- value
    nx2 <- length(x)

    if (nx1 < nx2) {
        names(x)[(nx1 + 1L):nx2] <- NA_character_
    }

    names <- names(i)
    if (!is.null(names)) {
        empty <- is.na(names) | !nzchar(names)
        names(x)[i[!empty]] <- names[!empty]
    }

    class(x) <- "record"
    x
}
