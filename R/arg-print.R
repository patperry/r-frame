arg_limit <- function(value, call = sys.call(-1), ...)
{
    if (is.null(value)) {
        return(NULL)
    }

    value <- arg_integer_scalar(value, name, call, ...)
    if (is.na(value)) {
        stop(simpleError(sprintf("%s cannot be NA", name), call))
    }

    value
}


