\name{schema}
\alias{schema}
\alias{schema.default}
\alias{schema.record}
\title{Schema}
\description{
Get the schema of a data object.
}
\usage{
schema(x)
}
\arguments{
\item{x}{an object.}
}
\details{
The \code{schema} function returns the type of a data object:
\enumerate{
    \item the schema of \code{NULL} is \code{NULL};

    \item the scheme of a vector-like argument \code{x} is \code{x};

    \item the schema of a record or dataset \code{x} is a record with the

    same names, with each field giving the schema of the corresponding
    entry in \code{x}

    \item the schema of a matrix-like argument \code{x} is the
    schema of \code{as.dataset(x)}.
}
Note that \code{schema(x)} is undefined for rank-3 and higher arrays.
}
\value{
A type (length-0 vector-like object) or a record with entries giving
the schema of the corresponding columns in \code{x}.
}
\seealso{
\code{\link{cast}}.
}
\examples{
# vector-like arguments
schema(letters)
schema(1:10)

# vector-like argument
x <- record(a = 1:10, b = LETTERS, c = FALSE)
schema(x)

# matrix-like
schema(iris)

# nested dataset
nest <- dataset(one = letters,
                two = 1:26,
                three = dataset(foo = rep(FALSE, 26),
                                bar = NULL))
schema(nest)
}
