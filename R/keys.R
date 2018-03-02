#  Copyright 2017 Patrick O. Perry.
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

keys <- function(x)
{
    UseMethod("keys")
}

keys.default <- function(x)
{
    NULL
}

keys.dataset <- function(x)
{
    attr(x, "dataset.keys", TRUE)
}

`keys<-` <- function(x, value)
{
    UseMethod("keys<-")
}


`keys<-.dataset` <- function(x, value)
{
    if (is.null(value)) {
        if (!is.null(keys(x))) {
            attr(x, "dataset.keys") <- NULL
        }
    } else {
        value <- as.keyset(value)

        n <- dim(x)[[1L]]
        nk <- dim(value)[[1L]]
        if (n != nk) {
            stop(sprintf("mismatch: keys have %.0f rows, data have %.0f",
                         nk, n))
        }

        attr(x, "dataset.keys") <- value
    }

    x
}


`keys<-.keyset` <- function(x, value)
{
    stop("setting 'keys' on a keyset object is not allowed")
}
