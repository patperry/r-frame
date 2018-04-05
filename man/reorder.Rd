\name{reorder.dataset}
\alias{reorder.dataset}
\title{Data Reordering}
\description{
Reorder the rows in a dataset.
}
\usage{
\S3method{reorder}{dataset}(x, ...)
}
\arguments{
\item{x}{a dataset object.}

\item{...}{a sequence of expressions that evaluate to vector-like
    or matrix-like values with the same number of rows as \code{x}.}
}
\details{
\code{reorder} reorders the rows of \code{x} according to the specified
expressions. These expressions get evaluated via the \code{scope} function
in a context that looks for symbols in \code{x} before the calling
environment.  After the arguments get evaluated the arguments
get converted to numeric via \code{xtfrm.dataset}, and then
the rows of \code{x} get sorted using these values.

By default, we sort in ascending order according to the arguments, in order.
To use descending order for one or more of the arguments use \code{desc =}
before the argument; see the examples below. You can also specify
\code{asc =}, but this has no effect as ascending order is the default.
}
\value{
A \code{dataset} with the rows of \code{x} put in the specified order.
}
\seealso{
\code{\link{order}}.
}
\examples{
x <- as.dataset(mtcars)

# order by 'cyl'
reorder(x, cyl)

# break ties using 'gear'
reorder(x, cyl, gear)

# order by 'cyl' descending, 'gear' ascending
reorder(x, desc = cyl, gear)

# order by 'cyl' descending, 'gear' descending
reorder(x, desc = cyl, desc = gear)
}
