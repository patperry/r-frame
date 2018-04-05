\name{sort.dataset}
\alias{sort.dataset}
\alias{xtfrm.dataset}
\title{Sorting Datasets}
\description{
Order the rows in a dataset.
}
\usage{
\method{sort}{dataset}(x, decreasing = FALSE, ...)
\method{xtfrm}{dataset}(x)
}
\arguments{
\item{x}{a dataset object.}
\item{decreasing}{whether to sort in decreasing order.}
\item{\dots}{ignored.}
}
\details{
The \code{sort.dataset} function converts its argument to a dataset and then
sorts its rows. With \code{decreasing = FALSE}, the default, sorts rows in
decreasing order; otherwise it sorts in increasing order.

The \code{xtfrm.dataset} function converts its argument to a dataset
and then returns a numerical vector which is ordered in the same way
as the rows of \code{x}.

In both cases, to determine the sort order, these functions first
transform vector-like columns of \code{x} via \code{xtfrm}, and
they transform matrix-like columns via \code{xtfrm.dataset}. Since
\code{xtfrm} is not defined for list columns, both \code{sort.dataset}
and \code{xtfrm.dataset} raise an error if \code{x} has a list column.
}
\value{
For \code{sort}, a dataset with the rows of \code{x} ordered in
ascending or descending order according to whether \code{decreasing} is
\code{FALSE}.

For \code{xtfrm}, a numeric vector with entries in the same order
as the rows of \code{x}.
}
\seealso{
\code{\link{reorder}}.
}
\examples{
x <- dataset(a = c(5, 4, 3, 2, 1, 2, 3, 4, 5),
             b = c(2, 2, 1, 1, 2, 2, 1, 1, 2))
sort(x)
sort(x, decreasing = TRUE)

xt <- xtfrm(x)
x[order(xt), ]
}
