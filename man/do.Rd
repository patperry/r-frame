\name{do}
\alias{do}
\alias{do.dataset}
\alias{do.default}
\title{Operating on Dataset Rows}
\description{
Perform an action on each row in a dataset and combine the results.
}
\usage{
do(x, f)
\method{do}{default}(x, f)
\method{do}{dataset}(x, f)
}
\arguments{
\item{x}{a data object.}

\item{f}{a function to perform on each row.}
}
\details{
The \code{do} function calls code \code{f} using each row
of \code{x} as a set of arguments. It then casts the result to
a record with \code{as.record} and binds the records together
into a dataset.
}
\value{
If \code{x} has no rows, then the result is \code{NULL}. Otherwise,
the result is a dataset with the same keys as \code{x} with rows
equal to the result of applying \code{f} to each row in \code{x}.
}
\seealso{
\code{\link{group}}, \code{\link{lapply}}.
}
\examples{
# perform an action on each row
x <- dataset(n = 1:10)
do(x, function(n) 10 * n)

# named result
do(x, function(n) c(a = 10 * n))

# multiple results
do(x, function(n)
   record(a =  10 * n,
          b = -10 * n,
          c = "foo"))

# multiple columns
y <- dataset(count = c("one", "two", "three"),
             fruit = c("banana", "banana", "orange"))
do(y, function(c, f) c(fun = paste(c, f)))

# split into groups
x <- group(mtcars, gear, cyl)

# perform an action on each group
do(x, function(g)
   record(n    = nrow(g),
          mean = mean(g$mpg),
          sd   = sd(g$mpg)))
}
