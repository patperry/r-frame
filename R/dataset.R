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


as.dataset.matrix <- function(x)
{
    dn <- dimnames(x)
    rownames <- dn[[1]]
    if (!is.null(rownames)) {
        rownames <- make.unique(rownames)
    }
    names <- dn[[2]]
    n <- d[[2]]

    if (n == 0) {
        mat <- matrix(0, d[[1]], 0)
        x <- as.data.frame(mat, rownames)
        x <- as.dataset(x)
        return(x)
    }

    cols <- vector("list", n)
    names(cols) <- names

    for (i in seq_len(n)) {
        xi <- x[, i, drop = TRUE]
        names(xi) <- NULL
        cols[[i]] <- xi
    }

    x <- as.record(cols)
    x <- as.dataset(x)

     if (!is.null(rownames)) {
        keys <- dataset(name = rownames)
        keys(x) <- keys
    }

    x
}


as.dataset.default <- function(x)
{
    if (is.dataset(x))
        return(x)

    d <- dim(x)
    r <- length(d)

    if (r == 2) {
        return(as.dataset.matrix(x))
    } else if (r > 2) {
        stop(sprintf("cannot convert rank-%.0f array to dataset", r))
    }

    name <- deparse(substitute(x), width.cutoff = 500L)
    rownames <- names(x)
    names(x) <- NULL
    col <- list(x)
    names(col) <- name

    x <- as.record(col)
    x <- as.dataset(x)

    if (!is.null(rownames)) {
        rownames <- make.unique(rownames)
        keys <- dataset(name = rownames)
        keys(x) <- keys
    }

    x
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
    nc <- length(x)
    nr <- rep_len(NA_integer_, nc)

    for (i in seq_len(nc)) {
        elt <- x[[i]]
        if (is.null(elt))
            next

        d <- dim(elt)
        r <- length(d)

        if (r <= 1) {
            nr[[i]] <- length(elt)
        } else if (r == 2) {
            nr[[i]] <- d[[1]]
        } else {
            names <- names(x)
            lab <- if (is.null(names)) "" else sprintf(" (\"%s\")", names[[i]])
            stop(sprintf("column %.0f%s has more than 2 dimensions", i, lab))
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

            fmt <- "mismatch: column %.0f has %.0f rows, column %.0f has %.0f"
            stop(sprintf(fmt, i1, nr1, i2, nr2))
        }
    }

    attr(x, "dataset.nrow") <- nr1
    class(x) <- c("dataset", class(x))

    x
}


col_name_paren <- function(x, i)
{
    names <- names(x)
    if (is.null(names)) {
        ""
    } else if (is.na(names[[i]])) {
        " (<NA>)"
    } else {
        paste0(" (\"", names[[i]], "\")")
    }
}
