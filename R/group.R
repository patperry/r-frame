

groups <- function(x, sort = TRUE)
{
    x <- as.normal.dataset(x)
    sort <- as.option(sort)
    .Call(rframe_groups, x, sort)
}
