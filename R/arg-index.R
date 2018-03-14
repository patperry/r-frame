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



arg_index <- function(value, n, names, get)
{
    nv <- length(value)
    if (nv != 1) {
        stop(sprintf("non-scalar index (length %.0f)", nv))
    }
    r <- length(dim(value))
    if (r >= 2) {
        stop(sprintf("non-scalar index (rank-%.0f array)", r))
    }

    if (is.object(value)) {
        if (is.numeric(value)) {
            value <- as.numeric(value)
        } else if (is.logical(value)) {
            value <- as.logical(value)
        } else {
            value <- as.character(value)
        }
    }

    if (is.numeric(value)) {
        i <- trunc(value)
        if (!isTRUE(1 <= i && i < Inf)) {
            stop(sprintf("invalid index (%s)", value))
        } else if (get && i > n) {
            stop(sprintf("bounds error: index is %.0f, maximum is %.0f", i, n))
        }
    } else if (is.character(value)) {
        value <- arg_names(value, "index")
        i <- match(value, names, 0L)
        if (i == 0) {
            if (get) {
                if (is.na(value)) {
                    stop("unknown name <NA>")
                } else {
                    stop(sprintf("unknown name \"%s\"", value))
                }
            } else {
                i <- n + 1L
                names(i) <- value
            }
        }
    } else {
        stop(sprintf("invalid index type (%s)", storage.mode(value)))
    }

    i
}
