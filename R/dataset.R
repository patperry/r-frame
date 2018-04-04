dataset <- function(...)
{
    cl <- sys.call()
    cl[[1]] <- quote(record)
    x <- eval(cl, parent.frame())
    as.dataset(x)
}


is.dataset <- function(x)
{
    inherits(x, "dataset")
}


dim.dataset <- function(x)
{
    nrow <- attr(x, "dataset.nrow", TRUE)
    n <- length(x)
    c(nrow, n)
}


dimnames.dataset <- function(x)
{
    names <- names(x)
    if (is.null(names))
        NULL
    else
        list(NULL, names)
}


as.dataset <- function(x)
{
    UseMethod("as.dataset")
}


make_dataset <- function(nrow, cols)
{
    x <- as.record(cols)
    attr(x, "dataset.nrow") <- nrow
    class(x) <- c("dataset", class(x))
    x
}


as.dataset.vector <- function(x)
{
    d <- dim(x)
    if (length(d) > 1)
        stop("argument is not a vector")

    nrow <- length(x)
    names(x) <- NULL
    cols <- list(x)

    make_dataset(nrow, cols)
}


as.dataset.matrix <- function(x)
{
    d <- dim(x)
    if (length(d) != 2)
        stop("argument is not a matrix")

    dn <- dimnames(x)
    names <- dn[[2]]
    nrow <- d[[1]]
    n <- d[[2]]

    cols <- vector("list", n)
    names(cols) <- names

    for (i in seq_len(n)) {
        xi <- x[, i, drop = TRUE]
        names(xi) <- NULL
        cols[[i]] <- xi
    }

    make_dataset(nrow, cols)
}


as.dataset.default <- function(x)
{
    if (is.null(x)) {
        return(make_dataset(0L, NULL))
    }

    d <- dim(x)
    r <- length(d)

    if (r <= 1) {
        if (is.list(x) && !is.object(x))
            as.dataset.record(x)
        else
            as.dataset.vector(x)
    } else if (r == 2) {
        as.dataset.matrix(x)
    } else {
        stop(sprintf("cannot convert rank-%.0f array to dataset", r))
    }
}


as.dataset.data.frame <- function(x)
{
    nrow <- nrow(x)
    x <- as.record(x)
    attr(x, "dataset.nrow") <- nrow
    class(x) <- c("dataset", class(x))
    x
}


dataset_nrow <- function(x)
{
    for (i in seq_along(x)) {
        elt <- x[[i]]
        if (is.null(elt))
            next

        if (is.record(elt) && !is.dataset(elt)) {
            nr <- dataset_nrow(elt)
            if (!is.na(nr))
                return(nr)
        } else {
            d <- dim(elt)
            r <- length(d)
            if (r <= 1) {
                return(length(elt))
            } else {
                return(d[[1]])
            }
        }
    }

    NA_integer_
}


cast_dataset <- function(x, nrow)
{
    for (i in seq_along(x)) {
        elt <- x[[i]]
        if (is.null(elt))
            next

        if (is.record(elt) && !is.dataset(elt)) {
            x[[i]] <- elt <- cast_dataset(elt, nrow)
            next
        }

        d <- dim(elt)
        r <- length(d)

        if (r <= 1) {
            nr <- length(elt)
        } else if (r == 2) {
            nr <- d[[1]]
        } else {
            stop(sprintf("column %.0f%s has more than 2 dimensions",
                         i, dataset_name(x, i)))
        }

        if (nr != nrow) {
            fmt <- "mismatch: column %.0f%s has %.0f rows, should have %.0f"
            stop(sprintf(fmt, i, dataset_name(x, i), nr, nrow))
        }
    }

    attr(x, "dataset.nrow") <- nrow
    class(x) <- c("dataset", class(x))
    x
}


as.dataset.record <- function(x)
{
    if (is.dataset(x))
        return(x)

    x  <- as.record(x)
    nc <- length(x)
    nrow <- dataset_nrow(x)
    if (is.na(nrow))
        nrow <- 1L

    cast_dataset(x, nrow)
}


dataset_name <- function(x, i)
{
    names <- names(x)
    if (is.null(names))
        return("")

    name <- names[[i]]
    if (is.na(name) || !nzchar(name))
        ""
    else
        paste0(" (`", name, "`)")
}
