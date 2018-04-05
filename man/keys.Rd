\name{keys}
\alias{keys}
\alias{keys<-}
\title{Data Keys}
\description{
Get or set the keys associated with a set of values.
}
\usage{
keys(x)
keys(x) <- value
}
\arguments{
  \item{x}{an object.}

  \item{value}{keys for \code{x}, or \code{NULL}.}
}
\details{
A \code{\link{dataset}} object and other similar objects can have a
\code{keys} attribute identifying the data values in the object. This
attribute is a dataset with unique rows, called a \code{\link{keyset}}.

Use \code{keys(x)} to retrieve the keys attribute for the rows in \code{x}.
Use \code{keys(x)<- value} to set attribute, or \code{keys(x) <- NULL} to
drop the keys.
}
\value{
    \code{keys(x)} is a keyset with the same number of rows as \code{x}.
}
\seealso{
\code{\link{dataset}}, \code{\link{keyset}}, \code{\link{lookup}}.
}
\examples{
# data.frame objects do not have keys
keys(mtcars)

# you can set keys on datasets
x <- as.dataset(mtcars)
keys(x) <- keyset(name = rownames(mtcars))

# 'keys' can be scalar...
keys(x) <- runif(32)

# ...or 'keys' can have multiple columns
keys(x) <- keyset(k1 = rep(LETTERS[1:4], each = 8),
                  k2 = rep(1:8, 4))

# set 'keys' to NULL to remove
keys(x) <- NULL
}
