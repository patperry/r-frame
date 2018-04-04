
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
    table <- as.keyset(table)
    rowid.keyset(table, x, default)
}


rowid.NULL <- function(table, x, default = NA)
{
    x <- as.dataset(x)
    default <- as.integer.scalar(default)
    default <- as.double(default)
    rep_len(default, nrow(x))
}


rowid.keyset <- function(table, x, default = NA)
{
    table   <- as.keyset(table)
    x       <- as.dataset(x)
    default <- as.integer.scalar(default)

    type    <- attr(table, "keyset.type", TRUE)
    x       <- cast(type, x)
    x       <- as.normal(x)
    default <- as.double(default)
    .Call(rframe_rowid_keyset, table, x, default)
}
