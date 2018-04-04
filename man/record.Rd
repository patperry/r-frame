\name{record}
\alias{record}
\alias{as.record}
\alias{as.record.default}
\alias{is.record}
\title{Record Objects}
\description{
Create or test for record objects.
}
\usage{
record(...)

as.record(x)

is.record(x)
}
\arguments{
\item{\dots}{record fields.}

\item{x}{object to be converted or tested.}
}
\details{
These functions create or test for \code{record} objects,
lists with different subscript and print behavior (see
\dQuote{Record Behavior} below for details).

\code{record} creates a new record object with fields taken from the
arguments to the method. Unnamed arguments get field names taken from
the quoted arguments.

\code{as.record} converts its argument to a record object. This is a generic
function. The default implementation converts the result of
\code{as.list} to a record.

\code{is.record} tests whether its argument is a valid record object.
}
\section{Record Behavior}{
Records behave like lists in many respects but they have different
subscripting and printing behavior.

Record subscript (\code{[}, \code{[[}, and \code{$}) behavior differs from
list behavior as follows:

\enumerate{
    \item index operations raise an error for unknown field names;

    \item the subscript operation \code{x[i]} for record object \code{x}
    and subscript \code{i} sets the names of the result from \code{names(i)}
    if they are present;

    \item with a logical subscript \code{i}, operation \code{x[i]} treats
    \code{NA} values in \code{i} like \code{FALSE};

    \item subscript with a logical value does not recycle its argument; 

    \item a \code{NULL} subscript is treated as a missing value;

    \item setting a field to \code{NULL} with double subscript (\code{[[})
    does not delete the field (delete with (\code{[}) instead).
}

Printing and formatting a record objects via \code{format} and \code{print}
produces different output than the corresponding methods for lists.
}
\value{
\code{record} and \code{as.record} return record objects.

\code{is.record} returns \code{TRUE} or \code{FALSE} depending on whether
its argument is a record.
}
\seealso{
\code{\link{c.record}, \link{print.record}, \link{dataset}}
}
\examples{
# construct a record; unnamed arguments get names from their quoted values
b <- "foo"
x <- record(a = 10, b, c = FALSE)
names(x)

# getting an unknown field raises an error
\dontrun{x$d}
\dontrun{x[[4]]}


# setting to NULL does not delete
x[[1]] <- NULL

# different printing
print(x)

# use single subscript (`[`) to delete
y <- x
y[1] <- NULL
print(y)

# index with logical treats NA like FALSE
x[c(FALSE, NA, TRUE)]

# no recycling subscripts
\dontrun{x[TRUE]}


# rename fields with a named subscript
x[c(first = "c", second = "b")]
x[c(first = 3,   second = 2)]
x[c(one = FALSE, two = TRUE, three = TRUE)]

# NULL treated as missing
identical(x[], x[NULL])
}
\keyword{classes}
