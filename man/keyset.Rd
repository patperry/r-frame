\name{keyset}
\alias{keyset}
\alias{as.keyset}
\alias{as.keyset.dataset}
\alias{as.keyset.default}
\alias{is.keyset}
\title{Keyset Objects}
\description{
Create or test for keyset objects.
}
\usage{
keyset(...)

as.keyset(x)

is.keyset(x)
}
\arguments{
\item{\dots}{keyset variables.}

\item{x}{object to be converted or tested.}
}
\details{
A keyset is a dataset of unique, simple values. Such a set is
used for the \code{keys} attribute of a dataset to identify the
rows.

The \code{keyset} function operates like the \code{dataset} function
by creating a keyset with the arguments as columns. The
\code{as.keyset} function coerces its argument to a keyset, and the
\code{is.keyset} tests whether its argument is one.

The code default behavior of \code{as.keyset} is to convert its
argument to a dataset with \code{as.dataset}, and then to convert
the result to simple with \code{as.simple}.

The \code{keyset} and \code{as.keyset} will fail if the result
cannot be converted with \code{as.simple} or if it does not have
unique rows.
}
\value{
\code{keyset} and \code{as.keyset} return keyset objects.

\code{is.keyset} returns \code{TRUE} or \code{FALSE} depending on
whether its argument is a keyset.
}
\seealso{
\code{\link{dataset}},
\code{\link{as.simple}},
\code{\link{lookup}}.
}
\examples{
# create a keyset
x <- keyset(k1 = c(1, 1, 2, 2, 3, 3),
            k2 = c(letters[1:3], letters[1:3]))
is.keyset(x)

# convert to a keyset
as.keyset(LETTERS)

# invalid: duplicate rows
\dontrun{x <- keyset(key = c(1, 2, 3, 1))}

# invalid: type cannot be converted to simple
\dontrun{val <- 1:3
class(val) <- "foo"
as.keyset(val)}
}
\keyword{classes}
