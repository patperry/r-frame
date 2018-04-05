\name{cast}
\alias{cast}
\alias{cast.default}
\title{Casting Types}
\description{
Convert a data object from one type to another.
}
\usage{
cast(type, x)
}
\arguments{
\item{type}{an object with the destination type.}
\item{x}{the source object.}
}
\details{
The \code{cast} function converts a data object from one type to
another. Its behavior is similar to that of the base type conversion
functions (\code{as.character}, \code{as.integer}, etc.), but it
allows for a dynamically-determined \code{type}.

The \code{type} argument can be any object with the desired
destination type such that \code{schema(type)} is well-defined. The
source \code{x} object must likewise be such that \code{schema(x)}
is well defined.

For the conversion to succeed, the number of components of \code{schema(x)}
must match that of \code{type(x)}.  Even so, if a value in the source object
cannot be represented in the destination type, then the cast operation will
raise a warning or error.

When casting to a record type, the names in the source object must either be
\code{NULL} or match the destination type.
}
\value{
The data object \code{x} converted to the destination \code{type}.
}
\seealso{
\code{\link{schema}}.
}
\examples{
# conversion to basic types
cast(integer(), c(TRUE, FALSE, NA))
cast(character(), c(TRUE, FALSE, NA))
cast(integer(), c(1.2, -99, NaN))

# conversion to factor
f <- factor(c(), levels = letters)
cast(f, c("a", "e", "i", "o", "u"))
cast(f, factor(c("a", "e", "i", "o", "u")))
\dontrun{cast(f, "A")} # invalid level; raises a warning

# conversion to record
r <- record(int = integer(), lgl = logical(), chr = character())
x <- record(a = 1.3, b = "TRUE", c = 3.14)

\dontrun{cast(r, x)} # fails, names don't match
cast(r, unname(x))   # succeed, source names are NULL
cast(unname(r), x)   # succeeds, destination names are NULL
names(x) <- names(r)
cast(r, x)           # succeeds, names match
}
