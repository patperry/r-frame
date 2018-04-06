\name{select}
\alias{select}
\alias{select.dataset}
\alias{select.default}
\title{Selecting Variables}
\description{
Select one or more expressions evaluated in the context of a data object.
}
\usage{
select(`_data`, ...)
\method{select}{dataset}(`_data`, ...)
\method{select}{default}(`_data`, ...)
}
\arguments{
\item{_data}{a data object.}
\item{...}{expressions to evaluate in the context of the data object.}
}
\details{
The \code{select} function evaluates a set of expressions in the context
of a data object, returning the results as a data object with the results
as variables.

The default implementation of the method coerces its \code{_data} argument
to a dataset, then uses \code{scope} to evaluate the expressions in the
context of the converted dataset. For the result, named arguments get these
names as variable names; unnamed arguments get the deparsed expressions as
names.

The expressions must evaluate to data objects with the same number of rows
as \code{_data}.

If the \code{_data} object has keys, then these get propagated to the result.
}
\value{
A data object with the evaluated expressions as variables.
}
\seealso{
\code{\link{transform.dataset}}, \code{\link{scope}}.
}
\examples{
# evaluate expressions, take given names
select(iris,
       Sepal.Area = Sepal.Length * Sepal.Width,
       Petal.Area = Petal.Length * Petal.Width)


# propagate keys, take names from arguments
x <- as.dataset(mtcars)
keys(x) <- keyset(name = rownames(mtcars))
select(x, cyl, 2 * cyl, three = 3 * cyl)
}
