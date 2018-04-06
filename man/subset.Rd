\name{subset.dataset}
\alias{subset.dataset}
\title{Subsetting Datasets}
\description{
Extract a subset of the rows of a dataset using a logical condition.
}
\usage{
\method{subset}{dataset}(x, subset, ...)
}
\arguments{
\item{x}{a dataset object.}
\item{subset}{a logical expression giving the subset condition.}
\item{...}{ignored.}
}
\details{
The \code{subset.dataset} first converts its first argument do a dataset and
then extracts the rows corresponding to the
\code{TRUE} elements in the \code{subset} argument. When evaluating the
\code{subset} condition, the method quotes the argument and then evaluates it
using \code{scope}, with the dataset-converted \code{x} as the first argument. 

The method is similar to \code{subset.data.frame}, but method, but it
lacks additional arguments beyond \code{subset}, and it uses a different
evaluation mechanism for the \code{subset} argument.
}
\value{
A dataset containing the rows in \code{x} corresponding to the \code{TRUE}
elements of the evaluated \code{subset} condition.
}
\seealso{
\code{\link{scope}}, \code{\link{subset.data.frame}}.
}
\examples{
x <- as.dataset(mtcars)
subset(x, cyl == 4)

# equivalent:
subset.dataset(mtcars, cyl == 4)

# scoped evaluation:
gear <- 1:nrow(mtcars)
subset(x, gear == 5)    # look for 'gear' in 'x', then the environment
subset(x, I(gear) == 5) # look for 'gear' in the environment
}
