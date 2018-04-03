\name{print.record}
\alias{format.dataset}
\alias{format.record}
\alias{print.dataset}
\alias{print.record}
\title{Format or Display}
\description{
Format or display a record or dataset object.
}
\usage{
\method{format}{dataset}(x, limit = NA, line = NA, control = NULL,
       meta = FALSE, ...)
\method{format}{record}(x, limit = NA, line = NA, control = NULL,
       meta = FALSE, ...)

\method{print}{dataset}(x, limit = NULL, line = NULL, control = NULL, ...)
\method{print}{record}(x, limit = NULL, line = NULL, control = NULL, ...)
}
\arguments{
    \item{x}{object to format or display.}
    \item{limit}{the maximum number of fields to format or display.}
    \item{line}{the maximum number of characters in a line.}
    \item{control}{additional formatting options (see \dQuote{Control} below.}
    \item{meta}{whether to include information that should used to format
       the result for display.}
    \item{\dots}{ignored.}
}
\details{
The \code{format.record} and \code{format.dataset} convert their argument
\code{x} to a record or dataset, respectively, and then return a record or
dataset with entries replaced by string representations suitable for display.

The \code{print.record} and \code{print.dataset} functions format their
arguments using the corresponding \code{format} method and then display the
results to the screen.

The \code{limit} argument sets the maximum number of fields or rows do display
for a record or dataset, respectively. Non-positive and \code{NA} values are
interpreted as no maximum. A \code{NULL} value is interpreted as the result of
\code{getOption("format.limit")}, or \code{20} if the latter is \code{NULL}.

The \code{line} argument specifies the width of a line, in characters. A
non-positive or \code{NA} value is interpreted as no limit. A \code{NULL}
value is interpreted as the result of \code{getOption("width")}.

The \code{control} argument is a list of formatting options, described in the
next section.
}
\section{Control}{
The print and format functions allow the user to specify additional
formatting options via the \code{conrol} argument, a list of named values.
These values include the following:
\itemize{
    \item \code{faint} and \code{bold}, ANSI SGR control codes
      specifying the format for faint and bold text for terminals
      that support ANSI styling;

    \item \code{tab}, the width (in spaces) of a tab;

    \item \code{pages}, the maximum number of pages of output to produce
      when line wrapping is required;

    \item \code{horiz2}, \code{ellipsis}, \code{vellipsis}, 
      and \code{vline}, characters to use for double horizontal lines,
      ellipsis, vertical ellipsis, and vertical line.
}
When the \code{control} argument is \code{NULL}, these values are taken
from \code{getOption("format.control")}. If that value is \code{NULL},
then \code{tab} and \code{pages} get taken from
\code{getOption("format.tab")} and \code{getOption("format.pages")};
otherwise 4 and 1 if either those values are \code{NULL}.
}
\value{
For \code{print.record} and \code{print.dataset}, return the argument \code{x}
after conversion to record or dataset, respectively.
}
\seealso{
\code{\link{record}}, \code{\link{dataset}}.
}
\examples{
# record
(x <- record(a = 1, b = "hello"))

# nested record
record(x, y = "hello", z = record(f = record(foo = TRUE, bar = FALSE)))

# dataset
(x <- as.dataset(mtcars))

print(x, NA) # no limit

# nested dataset
(x <- dataset(first = 1:32, second = rnorm(32), third = runif(32), mtcars))

print(x, control = list(pages = NA)) # no page limit
}
