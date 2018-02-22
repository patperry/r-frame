

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
