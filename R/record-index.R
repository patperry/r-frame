
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

    if (missing(i) || ((ni <- length(i)) == 0))
        stop("empty index")

    i1 <- arg_index(i[[1]], length(x), names(x), TRUE)
    entry <- .subset2(x, i1)

    if (ni > 1)
        entry[[ i[-1] ]]
    else
        entry
}



`[[<-.record` <- function(x, i, value)
{
    # TODO: implement in C

    if (missing(i) || ((ni <- length(i)) == 0))
        stop("empty index")

    if (ni == 1) {
        n <- length(x)
        names <- names(x)
        i1 <- arg_index(i[[1]], n, names, FALSE)

        class(x) <- NULL
        x[[i1]] <- value

        if (!is.null(value)) {
            nm <- names(i1)
            if (!is.null(nm)) {
                if (is.null(names)) {
                    names <- rep_len(NA, n)
                }
                names[[i1]] <- nm
                names(x) <- names
            } else if (i1 > n && !is.null(names)) {
                names(x) <- c(names, rep_len(NA, i1 - n))
            }
        }

        class(x) <- "record"
    } else {
        i1 <- i[[1]]
        entry <- x[[ i1 ]]
        entry[[ i[-1] ]] <- value
        x[[i1]] <- entry
    }

    x
}


`[.record` <- function(x, i)
{
    i <- arg_subscript(i, length(x), names(x), TRUE)
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
    i <- arg_subscript(i, length(x), names(x), FALSE)
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
        fmt <- "mismatch: replacement has %.0f entries, should have %.0f"
        stop(sprintf(fmt, nv, ni))
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
