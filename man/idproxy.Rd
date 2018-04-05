\name{idproxy}
\alias{idproxy}
\alias{idproxy.default}
\alias{idproxy.dataset}
\alias{idproxy.matrix}
\title{Identity Proxy}
\description{
Get an atomic representation of a data object.
}
\usage{
idproxy(x)
}
\arguments{
\item{x}{a data object.}
}
\details{
An \sQuote{identity proxy} is either an atomic vector or
a dataset of identity proxies. These proxies are used to
identify the distinct entries in \code{x} (or, for matrix-like \code{x}, the
distinct rows). Entries in the identity proxy are distinct if and only
if the corresponding entries (or rows) in \code{x} are distinct.

The default implementation is as follows:
\enumerate{
    \item If \code{x} is an atomic vector, then \code{idproxy(x)} is \code{x}.

    \item Otherwise, if \code{x} is vector-like, then \code{idproxy(x)} is
    \code{xtfrm(x)}.

    \item If \code{x} is matrix-like, then \code{idproxy(x)} is a dataset
    with columns equal to the identity proxies of the columns of \code{x}.
}
The \code{idproxy} should rarely (if ever) be used directly, but it is
used internally buy \code{unique.dataset} and the other similar functions.
}
\value{
An atomic vector or dataset of identity proxies used to identify the
distinct entries or rows in \code{x}.
}
\seealso{
\code{\link{unique.dataset}}, \code{\link{xtfrm.dataset}}.
}
\examples{
# atomic vectors are their own proxies
idproxy(1:20)
idproxy(letters)

# other vectors use xtfrm as a proxy
idproxy(as.hexmode(99:104))
idproxy(factor(letters))

# matrix-like objects have dataset idproxies
idproxy(matrix(1:20, 4, 5))
}
