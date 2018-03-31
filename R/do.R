
do <- function(x, f, type = NULL)
{
    UseMethod("do")
}


do.default <- function(x, f, type = NULL)
{
    x <- as.dataset(x)
    f <- as.function(f)
    do.dataset(x, f, type)
}


do.dataset <- function(x, f, type = NULL)
{
    x <- as.dataset(x)
    f <- as.function(f)

    n    <- nrow(x)
    keys <- keys(x)

    if (n == 0) {
        if (!is.null(type)) {
            y <- as.dataset(type)
            keys(y) <- keys(x)
        } else {
            y <- NULL
        }
        return(y)
    }

    y  <- vector("list", n)
    xt <- t(x)
    colnames(xt) <- NULL

    for (i in seq_len(n)) {
        yi <- do.call(f, xt[, i, drop = TRUE])
        yi <- as.dataset(yi)

        nr <- dim(yi)[[1]]
        if (nr != 1) {
            stop(sprintf("'do' action result %.0f had %.0f rows", i, nr))
        }

        y[[i]] <- yi
    }

    y <- do.call(rbind.dataset, y)
    if (!is.null(type)) {
        y <- cast(type, y)
        y <- as.dataset(y)
    }
    keys(y) <- keys

    y
}
