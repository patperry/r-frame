
lookup <- function(x, table, default = NA)
{
    rowid(table, x, default)
}


rowid <- function(table, x, default = NA)
{
    UseMethod("rowid")
}


rowid.default <- function(table, x, default = NA)
{
    match(x, table, default)
}


rowid.keyset <- function(table, x, default = NA)
{
    table   <- as.keyset(table)
    x       <- as.dataset(x)
    default <- as.integer.scalar(default)

    type    <- schema(table)
    x       <- cast(type, x)
    x       <- as.normal(x)
    default <- as.double(default)
    .Call(rframe_rowid_keyset, table, x, default)
}
