\name{dataset}
\alias{dataset}
\alias{as.dataset}
\alias{as.dataset.default}
\alias{as.dataset.data.frame}
\alias{as.dataset.matrix}
\alias{as.dataset.record}
\alias{is.dataset}
\title{Dataset Objects}
\description{
Create or test for dataset objects.
}
\usage{
dataset(...)

as.dataset(x)

is.dataset(x)
}
\arguments{
\item{\dots}{dataset variables.}

\item{x}{object to be converted or tested.}
}
\details{
These functions create or test for \code{dataset} objects,
sets of variables measured on the same individuals. These individuals
can optionally be identified by a set of keys, one for each individual.
A \code{dataset} is like a \code{data.frame} in many respects, but
exhibits some important differs (discussed below).

\code{dataset} creates a new dataset object with variables taken from the
arguments to the method. Unnamed arguments get variable names taken from
the quoted arguments.

\code{as.dataset} converts its argument to a dataset object. This is a generic
function. The default implementation works as follows:
\enumerate{
    \item A list or record argument gets converted to dataset of the same 
        length with the argument's fields as variables.

    \item A vector-like argument gets converted to single-variable dataset.

    \item A matrix-like argument gets converted to a dataset with columns of
        the argument as variables.
}
The default conversion fails for rank-3 and higher arrays.
}
\section{Dataset Behavior}{
Datasets are similar to \code{data.frame} objects but exhibit some
important differences:

\enumerate{
    \item Datasets are \code{record} objects and inherit their
    subscripting behavior for getting or setting columns.

    \item Datasets can optionally assign \code{keys} to each row identifying
    the measured individuals. These keys can be of almost-arbitrary type
    and can have multiple components; see \code{\link{keys}}.

    \item Key values can be used as row subscripts to extract specified
    rows. A row subscript that itself has keys propagates its own keys
    to the result.

    \item Datasets can contain both vector-like and matrix-like variables.
    In particular, datasets can be nested within other datasets.

    \item Subscripting operations default to \code{drop = FALSE}.

    \item Variable names are not constrained to be unique.  A dataset
    can have \code{NULL} names.

    \item Printing behavior is different; see \code{\link{print.dataset}}.
}
}
\value{
\code{dataset} and \code{as.dataset} return dataset objects.

\code{is.dataset} returns \code{TRUE} or \code{FALSE} depending on whether
its argument is a dataset.
}
\seealso{
\code{\link{cbind.dataset}}, \code{\link{rbind.dataset}},
\code{\link{print.dataset}}, \code{\link{record}}.
}
\examples{
x <- dataset(letters, x = runif(26), y = rpois(26, 1))

# get and rename a column
x[c(foo = "x")]

# set multi-component keys
keys(x) <- dataset(k1 = LETTERS, k2 = 1:26)

# index with keys
x[record(k1 = "E", k2 = 5), ]
k <- keys(x)[c(5, 17, 2, 1), ]
x[k, ]

# propagate subscript keys to result
keys(k) <- dataset(foobar = c(4, 2, 0, -2))
x[k, ]
}
\keyword{classes}
