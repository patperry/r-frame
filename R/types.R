#  Copyright 2018 Patrick O. Perry.
#
#  Licensed under the Apache License, Version 2.0 (the "License");
#  you may not use this file except in compliance with the License.
#  You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
#  Unless required by applicable law or agreed to in writing, software
#  distributed under the License is distributed on an "AS IS" BASIS,
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#  See the License for the specific language governing permissions and
#  limitations under the License.

## Types

as.option <- function(x)
{
    x <- as.scalar(x)
    as.logical(x)
}

as.enum <- function(choices, x)
{
    x <- as.character.scalar(x)
    i <- pmatch(x, choices, nomatch = 0L)
    if (i == 0L) {
        stop(sprintf("argument must be one of the following: %s",
                     paste(dQuote(choices), collapse = ", ")))
    }
    choices[[i]]
}

as.character.scalar <- function(x)
{
    x <- as.scalar(x)
    as.character.vector(x)
}

as.character.vector <- function(x)
{
    if (!is.character(x) || is.object(x))
        x <- as.character(x)

    x
}

as.integer.scalar <- function(x)
{
    x <- as.scalar(x)
    as.integer.vector(x)
}

as.integer.vector <- function(x)
{
    if (!is.integer(x) || is.object(x))
        x <- as.integer(x)

    x
}

as.mode <- function(x)
{
# A *mode* is a storage mode for a vector: any list or atomic mode.
    as.enum(c("any", "logical", "raw", "integer", "double",
              "complex", "character", "list"), x)
}

as.scalar <- function(x)
{
    n <- length(x)
    if (n != 1) {
        stop(sprintf("cannot coerce length-%.0f argument to a scalar", n))
    }
    x[[1]]
}

as.size.scalar <- function(x)
{
    x <- as.scalar(x)
    x <- as.numeric(x)

    if (is.na(x) || x < 0) {
        NA_integer_
    } else if (is.finite(x)) {
        x <- trunc(x)
        if (x <= .Machine$integer.max) {
            as.integer(x)
        } else {
            x
        }
    } else {
        stop(sprintf("argument must be finite or NA, not `%f`", x))
    }
}

as.vector.type <- function(x)
{
    if (!(length(x) == 0 && length(dim(x)) <= 1))
        stop("argument is not a vector type")
    x
}

as.vector.value <- function(x)
{
    if (is.record(x)) {
        nx <- length(x)
        if (nx != 1) {
            stop(sprintf("mismatch: type has 1 components, value has %.0f", nx))
        }
        x <- x[[1]]
        return(as.vector.value(x))
    }

    d <- dim(x)
    r <- length(d)

    if (r <= 1) {
        # pass
    } else if (r == 2) {
        nx <- d[[2]]
        if (nx != 1) {
            stop(sprintf("mismatch: type has 1 components, value has %.0f", nx))
        }
        x <- x[ , 1, drop = TRUE]
        x <- as.vector.value(x)
    } else if (r > 2) {
        stop(sprintf("cannot cast from rank-%.0f object to vector", r))
    }

    x
}
