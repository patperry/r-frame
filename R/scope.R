
scope <- function(x, expr, env = NULL)
{
    x    <- if (is.record(x) || is.data.frame(x)) x else as.list(x)
    expr <- substitute(expr)
    env  <- if (is.null(env)) parent.frame() else as.environment(env)

    if (is.call(expr)) {
        if (expr[[1]] == as.name("I")) {
            narg <- length(expr)
            if (narg == 2) {
                return(eval(expr[[2]], NULL, env))
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
                e <- call("scope", x, e, env)
                expr[[i]] <- e
            }
        }
    }

    eval(expr, x, env)
}
