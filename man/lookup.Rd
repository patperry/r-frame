\name{lookup}
\alias{lookup}
\alias{rowid}
\alias{rowid.default}
\alias{rowid.keyset}
\alias{rowid.NULL}
\title{Data Key Lookup}
\description{
Find rows with specified keys.
}
\usage{
lookup(x, table, default = NA)

rowid(table, x, default = NA)
}
\arguments{
\item{x}{the values to be matched.}

\item{table}{the values to be matched against.}

\item{default}{the value to return when no row
    in the \code{table} matches.}
}
\details{
The \code{lookup} and \code{rowid} functions to the same thing, but have their
arguments transposed.  Each finds the corresponding entry in the \code{table}
for each entry in \code{x}; for values in \code{x} with no match, return the
\code{default} value.

The \code{rowid} function is generic. The default implementation
converts \code{table} with \code{as.keyset} and converts \code{x}
with \code{as.dataset}, then finds the rows in the table corresponding
to the rows in \code{x}.

The \code{lookup} function calls the \code{rowid} generic.
}
\value{
A vector of row indices in \code{table} for the corresponding rows in the
\code{x} argument. Rows with no match get the \code{default} value (converted
to numeric).
}
\seealso{
\code{\link{keyset}}, \code{\link{match}}.
}
\examples{
# look up single-component keys
table <- keyset(name = rownames(mtcars))
lookup(c("Valiant", "Fiat 128", "Delorean"), table)

# rowid returns the same result but has its arguments swapped
rowid(table, c("Valiant", "Fiat 128", "Delorean"))

# multi-component keys
keys <- keyset(k1 = rep(LETTERS[1:4], each = 8), k2 = rep(1:8, 4))

# look up a single item
lookup(record(k1 = "A", k2 = 5), keys)

# look up multiple items
lookup(dataset(k1 = c("A", "A", "B", "Z"),
               k2 = c(  5, 100,   1,   3)), keys)
}
