\name{scope}
\alias{scope}
\alias{scopeQuoted}
\alias{scopeQuoted.default}
\alias{scopeQuoted.dataset}
\title{Scoped Evaluation}
\description{
Evaluate an expression in the context of a data environment.
}
\usage{
scope(x, expr, envir = NULL)

scopeQuoted(x, expr, envir = NULL)
}
\arguments{
\item{x}{a list or data object.}
\item{expr}{an object to be evaluated.}
\item{envir}{an enclosing environment, or \code{NULL} for the calling frame.}
}
\details{
The \code{scopeQuoted} evaluates its \code{expr} argument in a context that by
default looks for names first in \code{x} and then in \code{envir}.  If the
expression contains a call to \code{I()} then the argument of that call gets
evaluatedc in the environment \code{envir}, not in \code{x}.

The \code{scope} function quotes its \code{expr} argument and then calls
\code{scopeQuoted}.

The \code{scope} evaluation mechanism gets used in all functions in the
\sQuote{frame} package that require non-standard evaluation. It is
similar to the evaluation mechanism used by the \code{model.frame}
function, but is otherwise not used in base R.
}
\value{
The result of the evaluated expression \code{expr}.
}
\seealso{
\code{\link{select}}, \code{\link{transform.dataset}}.
}
\examples{
x <- list(a = 1:10, b = 10 * (1:10))
a <- 100000000
c <- 1000

# look up first in 'x', then in the calling environment
scope(x, a * b + c)

# look up 'a' in the calling environment, not in 'x'
scope(x, I(a) * b + c)

# evaluate a quoted expression
scopeQuoted(x, quote(a + c)) # equivalent to scope(x, a + c)
}
