\name{unique.dataset}
\alias{anyDuplicated.dataset}
\alias{anyDuplicated.keyset}
\alias{duplicated.dataset}
\alias{duplicated.keyset}
\alias{unique.dataset}
\alias{unique.keyset}
\title{Unique elements}
\description{
Get the unique rows in a dataset, or identify the duplicates.
}
\usage{
\method{anyDuplicated}{dataset}(x, incomparables = FALSE, ...)
\method{duplicated}{dataset}(x, incomparables = FALSE, ...)
\method{unique}{dataset}(x, incomparables = FALSE, ...)
}
\arguments{
\item{x}{a dataset object.}
\item{incomparables}{ignored.}
\item{\dots}{ignored.}
}
\details{
These functions convert their argument \code{x} to a dataset and identify
the unique rows. They do so by first transforming the columns of \code{x}
to identity proxies via the \code{idproxy} function.

Note that \code{idproxy} is not defined for \code{list} variables, so
consequently \code{anyDuplicated}, \code{duplicated}, and \code{unique}
will raise an error when \code{x} contains a list column.
}
\value{
For \code{anyDupicated}, the index of the first duplicated row, or 0
if all are unique.

For \code{duplicated}, a logical vector with length equal to the number
of rows in \code{x} and entries indicating whether or not each row
is a duplicate of an earlier row.

For \code{unique}, a dataset with rows giving the unique rows in \code{x}.
}
\seealso{
\code{\link{unique}}, \code{\link{idproxy}}.
}
\examples{
# identify duplicated elements
x <- dataset(one = c(  1,   1,   2,   2,   1,   1),
             two = c("a", "b", "a", "b", "a", "b"))
anyDuplicated(x)
duplicated(x)

# extract unique rows
unique(x)

# call the method explicitly to convert the argument
is.dataset(unique.dataset(mtcars))

# base-R 'unique' fails with R version < 3.5; 'unique.dataset' succeeds
x <- data.frame(first = c(.15, .10 + .05), second = c(1, 1))
unique(x$first)
unique(x)
unique.dataset(x)
}
