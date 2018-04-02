
transform.dataset <- function(`_data`, ...)
{
    `_data` <- as.dataset(`_data`)
    vars    <- substitute(dataset(...))
    vars    <- eval.parent(call("scope", `_data`, vars))

    names <- names(`_data`)
    varnames <- names(vars)
    if (!is.null(varnames) && !is.null(names)) {
        dst <- match(varnames, names)
        for (i in seq_along(dst)) {
            j <- dst[[i]]
            if (!is.na(j)) {
                `_data`[[j]] <- vars[[i]]
            }
        }
        vars <- vars[is.na(dst)]
    }

    cbind.dataset(`_data`, vars)
}
