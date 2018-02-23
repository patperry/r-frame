
scope <- function(x, expr, local = NULL, global = NULL)
{
    .NotYetImplemented()
    global <- if (is.null(global)) parent.frame() else as.environment(global)
}
