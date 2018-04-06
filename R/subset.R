
subset.dataset <- function(x, subset, ...)
{
    x <- as.dataset(x)
    subset <- substitute(subset)
    subset <- eval.parent(call("scope", x, subset))
    subset <- as.logical(subset)

    nrow <- nrow(x)
    ns <- length(subset)
    if (nrow != ns) {
        fmt <- "mismatch: data has %.0f rows, subset condition has %.0f"
        stop(sprintf(fmt, nrow, ns))
    }

    x[subset, ]
}
