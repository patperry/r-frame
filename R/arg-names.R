arg_names <- function(value, name)
{
    if (missing(value) || is.null(value))
        return(NULL)

    raw <- as.character(value)

    if (!all(utf8_valid(raw), na.rm = TRUE)) {
        invalid <- which(!utf8_valid(raw))[[1]]
        fmt <- "encoding error: %s entry %.0f (\"%s\")"
        stop(sprintf(fmt, name, invalid, raw[[invalid]]))
    }

    as_utf8(raw)
}
