
`$<-.dataset` <- function(x, name, value)
{
    x[[name]] <- value
    x
}


`[[<-.dataset` <- function(x, i, value)
{
    if (!is.null(value)) {
        d <- dim(value)
        r <- length(d)
        if (r <= 1) {
            nv <- length(value)
        } else if (r == 2) {
            nv <- d[[1]]
        } else {
            stop("replacement is not a vector or matrix")
        }

        nx <- attr(x, "dataset.nrow", TRUE)
        if (nx != nv) {
            fmt <- "mismatch: replacement has %.0f rows, data has %.0f"
            stop(sprintf(fmt, nv, nx))
        }
    }

    x <- NextMethod("[[<-")
    class(x) <- c("dataset", oldClass(x))

    x
}
