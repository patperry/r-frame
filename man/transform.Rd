\name{transform.dataset}
\alias{transform.dataset}
\title{Transform a Datasets}
\description{
Modify or add columns to a dataset.
}
\usage{
\method{transform}{dataset}(`_data`, ...)
}
\arguments{
\item{_data}{a dataset object.}
\item{...}{expressions used to modify or update the dataset.}
}
\details{
The \code{transform} function converts its first argument to a dataset,
then evaluates the quoted remaining arguments using \code{scope}. Named
arguments replace the corresponding columns in \code{x}, or get appended
if the names don't already exist; unnamed arguments get names taken from
their quoted values.
}
\value{
The modified dataset with the new columns.
}
\seealso{
\code{\link{select.dataset}}, \code{\link{scope}}.
}
\examples{
transform.dataset(mtcars, cyl = 2 * cyl) # modify an existing column

x <- as.dataset(iris)
transform(x, area = Sepal.Length * Sepal.Width) # add a new column
}
