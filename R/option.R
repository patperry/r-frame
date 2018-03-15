

option.frame.limit <- function()
{
    limit <- getOption("frame.limit")
    if (is.null(limit))
        20L
    else
        as.size.scalar(limit)
}


option.frame.line <- function()
{
    line <- getOption("frame.line")
    if (is.null(line))
        getOption("width")
    else
        as.size.scalar(line)
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
            wrap <- 1L
        }
    }
    wrap
}
