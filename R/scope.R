
scope <- function(x, expr, envir = NULL)
{
    expr  <- substitute(expr)
    envir <- if (is.null(envir)) parent.frame() else as.environment(envir)
    scopeQuoted(x, expr, envir)
}


scopeQuoted <- function(x, expr, envir = NULL)
{
    UseMethod("scopeQuoted")
}


scopeQuoted.default <- function(x, expr, envir = NULL)
{
    envir <- if (is.null(envir)) parent.frame() else as.environment(envir)
    expr  <- eval_I(expr, envir)
    eval(expr, x, envir)
}


scopeQuoted.dataset <- function(x, expr, envir = NULL)
{
    x     <- as.dataset(x)
    envir <- if (is.null(envir)) parent.frame() else as.environment(envir)
    scopeQuoted.default(x, expr, envir)
}


eval_I <- function(expr, envir)
{
    if (!is.call(expr))
        return(expr)

    if (expr[[1]] == as.name("I")) {
        narg <- length(expr)
        if (narg == 2) {
            return(eval(expr[[2]], NULL, envir))
        } else if (narg == 1) {
            stop("empty I() call")
        } else {
            stop(sprintf("too many arguments (%.0f) to I()", narg - 1))
        }
    } else {
        for (i in seq_along(expr)) {
            e <- expr[[i]]
            if (!is.call(e))
                next
            expr[[i]] <- eval_I(e, envir)
        }
    }

    expr
}
