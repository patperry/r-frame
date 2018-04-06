
select <- function(`_data`, ...)
{
    UseMethod("select")
}


select.default <- function(`_data`, ...)
{
    `_data` <- as.dataset(`_data`)
    vars    <- substitute(cbind.dataset(...))
    vars    <- eval.parent(call("scope", `_data`, vars))

    select.dataset(`_data`, I(vars))
}


select.dataset <- function(`_data`, ...)
{
    `_data` <- as.dataset(`_data`)
    vars    <- substitute(cbind.dataset(...))
    vars    <- eval.parent(call("scope", `_data`, vars))

    if (is.null(vars)) {
        return(`_data`[0])
    }

    nx <- nrow(`_data`)
    nv <- nrow(vars)
    if (nx != nv) {
        stop(sprintf("mismatch: data have %.0f rows, selected values have %.0f",
                     nx, nv))
    }

    keys(vars) <- keys(`_data`)
    vars
}
