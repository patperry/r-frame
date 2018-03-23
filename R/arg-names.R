arg_names <- function(value, name)
{
    if (missing(value) || is.null(value))
        return(NULL)

    raw <- as.character(value)
    raw[!nzchar(raw)] <- NA

    if (!all(utf8_valid(raw), na.rm = TRUE)) {
        invalid <- which(!utf8_valid(raw))[[1]]
        fmt <- "encoding error: %s entry %.0f (\"%s\")"
        stop(simpleError(sprintf(fmt, name, invalid, raw[[invalid]]), call))
    }

    as_utf8(raw)
}
