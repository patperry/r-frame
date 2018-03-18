

as.limit <- function(x)
{
    if (is.null(x))
        x <- getOption("frame.limit")
    if (is.null(x))
        20L
    else
        as.size.scalar(x)
}


as.pages <- function(x)
{
    if (is.null(x))
        x <- getOption("frame.pages")
    if (is.null(x))
        1L
    else
        as.size.scalar(x)
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
    if (is.null(x)) {
        2L
    } else {
        x <- as.size.scalar(x)
        if (is.na(x))
            2L
        else
            x
    }
}


as.style <- function(x)
{
    if (is.null(x))
        x <- getOption("frame.style")

    if (is.null(x)) {
        # RStudio has only partial ANSI support
        # https://github.com/rstudio/rstudio/issues/1721
        #
        #  + no ANSI faint; use gray instead
        #  + no ANSI bold; use color instead
        #
        record(faint = "38;5;246", #666666
               bold  = "38;5;203", #FF3333
               line  = getOption("width"),
               tab   = 2L)
    } else {
        x <- as.list(x)[c("faint", "bold", "line", "tab")]
        x["faint"] <- as.ansi(x$faint)
        x["bold"]  <- as.ansi(x$bold)
        x["line"]  <- as.line(x$line)
        x["tab"]   <- as.tab(x$indent)
        as.record(x)
    }
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


option_rows <- function(rows)
{
    if (is.null(rows)) {
        rows <- getOption("frame.rows")
        if (is.null(rows)) {
            rows <- 20L
        }
    }
    rows
}


option_wrap <- function(wrap)
{
    if (is.null(wrap)) {
        wrap <- getOption("frame.wrap")
        if (is.null(wrap)) {
            wrap <- 0L
        }
    }
    wrap
}
