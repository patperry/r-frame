
select <- function(`_data`, ...)
{
    UseMethod("select")
}


select.default <- function(`_data`, ...)
{
    call <- sys.call()
    call[[1]] <- quote(select.dataset)
    eval(call, parent.frame())
}


select.dataset <- function(`_data`, ...)
{
    `_data` <- as.dataset(`_data`)
    vars    <- substitute(dataset(...))
    vars    <- eval.parent(call("scope", `_data`, vars))

    if (length(vars) == 0) {
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
