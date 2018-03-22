

as.limit <- function(x)
{
    if (is.null(x))
        x <- getOption("format.limit")
    if (is.null(x)) {
        20L
    } else {
        x <- as.size.scalar(x)
        if (!is.na(x) && x <= 0) {
            x <- NA_integer_
        }
        x
    }
}


as.pages <- function(x)
{
    if (is.null(x))
        x <- getOption("format.pages")
    if (is.null(x)) {
        1L
    } else {
        x <- as.size.scalar(x)
        if (!is.na(x) && x <= 0) {
            x <- NA_integer_
        }
        x
    }
}


as.line <- function(x)
{
    if (is.null(x)) {
        getOption("width")
    } else {
        as.size.scalar(x)
    }
}


as.tab <- function(x)
{
    if (is.null(x))
        x <- getOption("format.tab")
    if (is.null(x)) {
        2L
    } else {
        x <- as.size.scalar(x)
        if (is.na(x))
            0L
        else
            x
    }
}


as.format.control <- function(x)
{
    if (is.null(x))
        x <- getOption("format.control")

    if (is.null(x)) {
        # RStudio has only partial ANSI support
        # https://github.com/rstudio/rstudio/issues/1721
        #
        #  + no ANSI faint; use gray instead
        #  + no ANSI bold; use color instead
        #
        x <- record(
            faint  = "38;5;246", #666666
            bold   = "38;5;203", #FF3333
            line   = as.line(NULL),
            tab    = as.tab(NULL),
            pages  = as.pages(NULL),
            horiz2    = "\u2550",
            ellipsis  = "\u2026",
            vellipsis = "\u22ee",
            vline     = "\u2502")
    } else {
        x <- as.list(x)[c("faint", "bold", "line", "tab", "pages",
                          "horiz2", "ellipsis", "vellipsis", "vline")]
        x$faint  <- as.ansi(x$faint)
        x$bold   <- as.ansi(x$bold)
        x$line   <- as.line(x$line)
        x$tab    <- as.tab(x$tab)
        x$pages  <- as.pages(x$pages)
        x$horiz2    <- if (is.null(x$horiz2)) "\u2550"
                       else as.character.scalar(x$horiz2)
        x$ellipsis  <- if (is.null(x$ellipsis)) "\u2026"
                       else as.character.scalar(x$ellipsis)
        x$vellipsis <- if (is.null(x$vellipsis)) "\u22ee"
                       else as.character.scalar(x$vellipsis)
        x$vline     <- if (is.null(x$vline)) "\u2502"
                       else as.character.scalar(x$vline)
        x <- as.record(x)
    }

    x$horiz2    <- utf8_fallback(x$horiz2, "=")
    x$ellipsis  <- utf8_fallback(x$ellipsis, "...")
    x$vellipsis <- utf8_fallback(x$vellipsis, ".")
    x$vline     <- utf8_fallback(x$vline, "|")
    x
}


as.ansi <- function(x)
{
    if (is.null(x))
        return(NA_character_)

    x <- as.character.scalar(x)
    if (is.na(x))
        return(x)

    if (!grepl("^[0-9;]*$", x)) {
        stop("argument must be a valid ANSI SGR parameter string")
    }
    if (nchar(x) >= 128) {
        stop("argument must have length below 128 characters")
    }
    x
}
