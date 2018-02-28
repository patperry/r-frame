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
        return(make_dataset(1, NULL))
    }

    d <- dim(x)
    r <- length(d)

    if (r <= 1) {
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
    keys <- if (.row_names_info(x) > 0)
        dataset(name = row.names(x))
    else NULL

    x <- as.record(x)
    attr(x, "dataset.nrow") <- nrow
    class(x) <- c("dataset", class(x))
    keys(x) <- keys

    x
}


as.dataset.record <- function(x)
{
    if (is.dataset(x))
        return(x)

    nc <- length(x)
    nr <- rep_len(NA_integer_, nc)

    for (i in seq_len(nc)) {
        elt <- x[[i]]

        if (is.record(elt)) {
            elt <- as.dataset(elt)
            if (length(elt) == 0) {
                elt <- NULL
                x[i] <- list(NULL)
            } else {
                x[[i]] <- elt
            }
        }

        if (is.null(elt))
            next

        d <- dim(elt)
        r <- length(d)

        if (r <= 1) {
            nr[[i]] <- length(elt)
        } else if (r == 2) {
            nr[[i]] <- d[[1]]
        } else {
            stop(sprintf("column %.0f%s has more than 2 dimensions",
                         i, dataset_name(x, i)))
        }
    }

    nr1 <- NA
    for (i1 in seq_len(nc)) {
        nr1 <- nr[[i1]]
        if (!is.na(nr1))
            break
    }

    if (is.na(nr1)) {
        nr1 <- 0L
    } else if (i1 < nc) {
        for (i2 in (i1 + 1):nc) {
            nr2 <- nr[[i2]]
            if (is.na(nr2) || nr2 == nr1)
                next

            fmt <- "mismatch: column %.0f%s has %.0f rows, column %.0f%s has %.0f"
            stop(sprintf(fmt,
                         i1, dataset_name(x, i1), nr1,
                         i2, dataset_name(x, i2), nr2))
        }
    }

    attr(x, "dataset.nrow") <- nr1
    class(x) <- c("dataset", class(x))

    x
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
