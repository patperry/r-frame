\name{cbind.dataset}
\alias{cbind.dataset}
\alias{rbind.dataset}
\title{Combining Columns or Rows}
\description{
Create a new dataset by concatenating columns or rows together.
}
\usage{
\method{cbind}{dataset}(..., deparse.level = 1)
\method{rbind}{dataset}(..., deparse.level = 1)
}
\arguments{
\item{\dots}{objects to concatenate.}
\item{deparse.level}{ignored.}
}
\details{
The \code{cbind.dataset} treats its arguments as columns or
sets of columns, and binds them together into a single dataset.
The \code{rbind.dataset} acts similarly, but treats its arguments as rows.

For both functions, \code{NULL} arguments get ignored. If there are
no non-\code{NULL} arguments, then the result is \code{NULL}.

As with \code{c.record}, we explicitly export the method instances
to allow control over method dispatch. A call to the generic
\code{cbind} or \code{rbind} function will only dispatch to the
dataset instance if all arguments are atomic or datasets. Call
\code{cbind.dataset} or \code{rbind.dataset} explicitly if you
want the dataset instance of one of these methods.
}
\section{Binding Columns}{
For \code{cbind.dataset}, vector-like arguments get converted to
single-column datasets, and matrix-like arguments get converted to
datasets.

If argument names are provided, then these get used for the vector-like
arguments, or they become prefixes (with \code{.}) of matrix-like arguments.
Otherwise, unnamed vector-like arguments get their names taken from the quoted
arguments.

All arguments must have the same number of rows. If any arguments have
non-\code{NULL} keys, then these get propagated to the result. If two or more
arguments have non-\code{NULL} keys, then they must be identical.
}
\section{Binding Rows}{
For \code{rbind.dataset}, vector-like arguments get converted to
single-row datasets, and matrix-like arguments get converted to datasets.

Argument names are not allowed.

All arguments must have the same number of columns. If any arguments have
non-\code{NULL} names, then these get propagated to the result. If two or more
arguments have non-\code{NULL} names, then they must be identical.
}
\value{
A dataset with columns or rows taken from the non-\code{NULL} arguments, or
\code{NULL} if there are no non-\code{NULL} arguments.
}
\seealso{
\code{\link{dataset}}, \code{\link{c.record}}.
}
\examples{
# bind columns together
x <- 1:5
y <- letters[1:5]
z <- dataset(foo = rnorm(5), bar = logical(5))

cbind.dataset(x, y, z) # vector names taken from arguments
cbind.dataset(a = x, b = y, c = z) # alternative names

# bind rows together
rbind.dataset(x, y)

rbind.dataset(mtcars[1:3, ], 1:11, mtcars[20:25, ])

# propagate names to the result
names(y) <- LETTERS[1:5]
rbind.dataset(x, y)

# mismatched names result in an error
names(x) <- LETTERS[5:1]
\dontrun{rbind.dataset(x, y)}
}
