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


## Normal Objects

# A *normal* object is a list, or vector containing normalized values.
# Normalization places restrictions on the following types:
#
#   + `list` (or `object`), all entries are normal;
#
#   + `character`, all values are encoded in normalized UTF-8
#     and `NA` is not allowed (`""` is acceptable);
#
#   + `double`, the `NA` value is not allowed (`NaN` is acceptable);
#
#   + `complex`, the real and imaginary parts are normal double values.
#

is.normal <- function(x)
{
    if (is.null(x) || inherits(x, "normal")) {
        TRUE
    } else if (is.vector(x) || (is.object(x) && isTRUE(length(dim(x)) <= 1))) {
        type <- typeof(x)
        if (type == "list")
            isTRUE(all(logical.map(x, is.normal)))
        if (type == "character")
            isTRUE(all(x == utf8_normalize(x)))
        else if (type == "double" || type == "complex")
            !anyNA(x) || all(is.nan(x[is.na(x)]))
        else if (type == "logical")
            !anyNA(x)
        else
            TRUE
    } else {
        FALSE
    }
}

# When coercing to a normal object, we replace invalid values with
# their normalized equivalents.

as.normal <- function(x)
    UseMethod("as.normal")

as.normal.default <- function(x)
{
    if (is.null(x) || inherits(x, "normal"))
        return(x)

    type <- typeof(x)
    if (type == "character") {
        as.normal.character(x)
    } else if (type == "double") {
        as.normal.double(x)
    } else {
        class(x) <- c("normal", oldClass(x))
        x
    }
}

# For `character`, we convert values to normalized UTF-8,
# and replace `NA` with `""`.

as.normal.character <- function(x)
    UseMethod("as.normal.character")

as.normal.character.default <- function(x)
{
    x <- as.character(x)
    if (inherits(x, "normal"))
        return(x)

    normal.x <- utf8_normalize(x)
    if (!identical(x, normal.x)) {
        x <- normal.x
        if (anyNA(x))
            x[is.na(x)] <- ""
    }

    class(x) <- c("normal", oldClass(x))
    x
}

# For `double`, we replace `NA` with `NaN`.

as.normal.double <- function(x)
    UseMethod("as.normal.double")

as.normal.double.default <- function(x)
{
    x <- as.double(x)
    if (inherits(x, "normal"))
        return(x)

    if (anyNA(x))
        x[is.na(x)] <- NaN

    class(x) <- c("normal", oldClass(x))
    x
}

# For `complex`, we mimic the behavior for `double`.

as.normal.complex <- function(x)
    UseMethod("as.normal.complex")

as.normal.complex.default <- function(x)
{
    x <- as.complex(x)
    if (inherits(x, "normal"))
        return(x)

    if (anyNA(x)) {
        if (anyNA(Re(x)))
            Re(x)[is.na(Re(x))] <- NaN

        if (anyNA(Im(x)))
            Im(x)[is.na(Im(x))] <- NaN
    }

    class(x) <- c("normal", oldClass(x))
    x
}
