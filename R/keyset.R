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

keyset <- function(...)
{
    cl <- sys.call()
    cl[[1L]] <- quote(dataset)
    x <- eval(cl, parent.frame())
    as.keyset(x)
}


is.keyset <- function(x)
{
    inherits(x, "keyset")
}


as.keyset <- function(x)
{
    UseMethod("as.keyset")
}


as.keyset.default <- function(x)
{
    if (is.keyset(x))
        return(x)

    x <- as.dataset(x)
    as.keyset(x)
}


as.keyset.dataset <- function(x)
{
    x <- as.dataset(x)
    keys(x) <- NULL

    y <- as.normal(x)
    u <- .Call(rframe_unique, y)

    if (length(u$types) < length(u$group)) {
        j <- anyDuplicated(u$group)
        stop(sprintf("argument has a duplicate row (%.0f)", j))
    }

    attr(x, "keyset.type")   <- schema(y)
    attr(x, "keyset.normal") <- y
    attr(x, "keyset.hash")   <- u$hash
    attr(x, "keyset.table")  <- u$table
    class(x) <- c("keyset", class(x))
    x
}


as.keyset.keyset <- function(x)
{
    x
}
