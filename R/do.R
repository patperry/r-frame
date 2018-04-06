
do <- function(x, f)
{
    UseMethod("do")
}


do.default <- function(x, f)
{
    x <- as.dataset(x)
    do.dataset(x, f)
}


do.dataset <- function(x, f)
{
    x <- as.dataset(x)
    f <- as.function(f)

    n <- nrow(x)
    if (n == 0) {
        return(NULL)
    }

    y  <- vector("list", n)
    xt <- t(x)
    rownames(xt) <- NULL

    for (i in seq_len(n)) {
        yi <- do.call(f, xt[, i, drop = TRUE])
        yi <- as.record(yi)
        y[[i]] <- yi
    }

    y <- do.call(rbind.dataset, y)
    keys(y) <- keys(x)
    y
}
