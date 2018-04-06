\name{group}
\alias{group}
\alias{group.dataset}
\alias{group.default}
\title{Grouped Data}
\description{
Group the rows in a dataset according to a specified factor or set of factors.
}
\usage{
group(`_data`, ...)
\method{group}{default}(`_data`, ...)
\method{group}{dataset}(`_data`, ...)
}
\arguments{
\item{_data}{object to be grouped.}

\item{\dots}{grouping factors for the rows.}
}
\details{
\code{group} is a generic method that splits the rows in a data set
according to a grouping factor (the additional \code{...} arguments).
These arguments get evaluated in the scope of the \code{_data} argument
using the \code{scope} function.

Commonly, the next operation after a call to \code{group} will be
a call to \code{do} that operates on the groups of rows.
}
\value{
A single-variable dataset with rows corresponding to the distinct combinations
of the grouping factors. Each list entry is a dataset of rows that share
the same grouping factor values.  The keys of the result are the grouping
factors.
}
\seealso{
\code{\link{do}}, l\code{\link{scope}}, \code{\link{split}}.
}
\examples{
# split into groups
x <- group(mtcars, gear, cyl)

# entries share the same grouping factors
x[[1]][[1]]

x[[1]][[2]]
}
