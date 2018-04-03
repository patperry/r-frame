\name{c.record}
\alias{c.record}
\title{Concatenate Records}
\description{
Create a new record by concatenating values together.
}
\usage{
\method{c}{record}(...)
}
\arguments{
\item{\dots}{values to concatenate.}
}
\details{
The \code{c.record} function converts its non-\code{NULL} arguments
to records with \code{as.record}, then forms a new record by concatenating
the records together.

Named arguments get their field names qualified with the specified argument
name, separated by a period (\code{.}).

Calling the \code{c} generic function with a record as the first argument
will dispatch to \code{c.record}. We export the \code{c.record} function
explicitly to enable control over the dispatch with arbitrary argument types.
}
\value{
A record gotten by concatenating the non-\code{NULL} argument fields together,
or \code{NULL} if no arguments are non\code{NULL}.
}
\seealso{
\code{\link{record}}, \code{\link{cbind.dataset}}.
}
\examples{
x <- record(a = 1, b = "two")
y <- 3
z <- list(d = FALSE)

# record as first argument
c.record(x, y, z) # explicit dispatch
c(x, y, z)        # implicit dispatch (same result)

# non-record as first argument
c.record(z, y, x) # explicit dispatch
c(z, y, x)        # implicit dispatch (different result)

# named arguments
c.record(xx = x, yy = y, zz = z)
}
