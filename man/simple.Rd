\name{simple}
\alias{as.simple}
\alias{as.simple.character}
\alias{as.simple.complex}
\alias{as.simple.dataset}
\alias{as.simple.Date}
\alias{as.simple.default}
\alias{as.simple.double}
\alias{as.simple.factor}
\alias{as.simple.integer}
\alias{as.simple.logical}
\alias{as.simple.matrix}
\alias{as.simple.NULL}
\alias{as.simple.numeric}
\alias{as.simple.POSIXt}
\alias{as.simple.raw}
\title{Simple Data Objects}
\description{
Convert a data object to simple form.
}
\usage{
as.simple(x)
}
\arguments{
\item{x}{object to be converted.}
}
\details{
Simple data objects are the only values allowed in a keyset. Plain
atomic vectors are simple, subject to certain restrictions on the values
described below, as are certain object vectors including
\code{factor}, \code{Date}, and \code{POSIXct}. A dataset is simple
whenever all its columns are simple.

The \code{as.simple} function converts its argument to a simple
data object or fails with an error if no conversion is possible.

The default implementation of \code{as.simple} splits a matrix-like
object into a dataset of columns and calls \code{as.simple} on each
column. Plain atomic vectors get converted as follows:

\itemize{
  \item \code{double}: negative zero (\code{-0}) gets
    converted to zero; all non-\code{NA} not-a-numbjer \code{NaN}
    values get converted to the canonical \code{NaN}.

  \item \code{character}: all entries not in \code{"bytes"}
    encoding get converted to ASCII (\code{"unknown"}) or
    \code{"UTF-8"} encoding.

  \item \code{complex}: the real and imaginary parts get
    converted to simple double; all \code{NA} values get
    converted to the canonical complex \code{NA}.
}

Objects with class \code{Date}, \code{factor}, and \code{POSIXct}
have their underlying storage converted to simple. Objects
with class \code{POSIXlt} get converted to simple \code{POSIXct}

Calls to \code{as.simple} with a list object \code{x} or an object
without an implementation of \code{as.simple} will raise an error.
}
\value{
The object \code{x} converted to a simple data object.
}
\seealso{
\code{\link{keyset}}.
}
\examples{
# objects that are already simple stay unchanged
as.simple(NULL)
as.simple(1:10)
as.simple(factor(letters))

# double: replace -0 by 0 (also, replace non-canonical NaN with NaN)
x <- c(1.3, 0, -0)
y <- as.simple(x)
1 / x
1 / y

# POSIXlt get converted to simple POSIXct
time <- as.POSIXlt(Sys.time())
class(time)
class(as.simple(time))

# matrix-like objects get converted to datasets of columns
as.simple(matrix(1:20, 4, 5))
as.simple(iris)
}
