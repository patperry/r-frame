
subset.dataset <- function(x, ...)
{
    x <- as.dataset(x)
    exprs <- substitute(list(...))
    exprs <- eval.parent(call("scope", x, exprs))

    n <- length(exprs)
    if (n == 0)
        return(x)

    nrow <- nrow(x)

    for (i in seq_len(n)) {
        e <- exprs[[i]]
        e <- as.logical(e)

        ne <- length(e)
        if (nrow != ne) {
            fmt <- "mismatch: data has %.0f rows, condition length is %.0f"
            stop(sprintf(fmt, nrow, ne))
        }

        if (i == 1) {
            mask <- e
        } else {
            mask <- mask & e
        }
    }

    x[mask, ]
}
