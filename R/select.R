
select <- function(`_data`, ...)
{
    UseMethod("select")
}


select.default <- 
select.dataset <- function(`_data`, ...)
{
    `_data` <- as.dataset(`_data`)
    vars    <- substitute(dataset(...))
    vars    <- eval.parent(call("scope", `_data`, vars))

    nx <- nrow(`_data`)
    nv <- nrow(vars)
    if (nx != nx) {
        stop(sprintf("mismatch: data have %.0f rows, selected values have %.0f",
                     nx, nv))
    }

    keys(vars) <- keys(`_data`)
    vars
}
