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


arg_record_names <- function(n, value, name, call = sys.call(-1))
{
    if (missing(value) || is.null(value))
        return(NULL)

    raw <- as.character(value)
    raw[!nzchar(raw)] <- NA

    names <- tryCatch(as_utf8(raw), error = function(cond) NULL)
    if (is.null(names)) {
        invalid <- which(!utf8_valid(raw))[[1]]
        fmt <- "%s entry %.0f has wrong character encoding"
        stop(simpleError(sprintf(fmt, name, invalid), call))
    }

    if (n >= 0) {
        nvalue <- length(names)
        if (nvalue != n) {
            fmt <- "mismatch: %s length is %.0f, object length is %.0f"
            stop(simpleError(sprintf(fmt, name, nvalue, n), call))
        }
    }

    names
}


arg_record_subset <- function(x, value, strict = FALSE, call = sys.call(-1))
{
    if (missing(value) || is.null(value)) {
        return(NULL)
    }
    
    if (length(dim(value)) >= 2) {
        fmt <- "cannot index with rank-%.0f array"
        stop(simpleError(sprintf(fmt, length(dim(value))), call))
    }
    
    if (is.numeric(value)) {
        if (is.object(value)) {
            names <- names(value)
            value <- as.numeric(value)
            names(value) <- names
        }

        if (!is.integer(value)) {
            value <- trunc(value)
        }

        if (anyNA(value)) {
            stop(simpleError("numeric index cannot contain NA values", call))
        }

        if (length(value) == 0) {
            # pass
        } else if (isTRUE(value[[1]] < 0)) {
            if (!all(value < 0)) {
                stop(simpleError(
                     "numeric index cannot contain both negative and non-negative values",
                     call))
            }
            value <- seq_along(x)[value]
        } else {
            if (!all(value >= 0)) {
                stop(simpleError(
                     "numeric index cannot contain both negative and non-negative values",
                     call))
            }
            if (strict) {
                n <- length(x)
                bounds <- value > n
                if (any(bounds)) {
                    i <- which(bounds)[[1]]
                    vi <- value[[i]]
                    fmt <- "bounds error: index is %.0f, maximum is %.0f"
                    stop(simpleError(sprintf(fmt, vi, n), call))
                }
            }
        }

        value
    } else if (is.logical(value)) {
        if (is.object(value)) {
            names <- names(value)
            value <- as.logical(value)
            names(value) <- names
        }

        n <- length(x)
        nvalue <- length(value)
        if (nvalue != n) {
            fmt <- "mismatch: logical mask length is %.0f, object length is %.0f"
            stop(simpleError(sprintf(fmt, nvalue, n), call))
        }

        which(value)
    } else {
        names <- names(value)
        if (!is.character(value) || is.object(value)) {
            value <- as.character(value)
        }

        if (is.null(names)) {
            names <- value
        } else {
            empty <- is.na(names) | !nzchar(names)
            names[empty] <- value[empty]
        }

        index <- match(value, names(x), 0)
        names(index) <- names

        new <- which(index == 0)
        nnew <- length(new)
        if (nnew > 0) {
            if (strict) {
                i <- new[[1]]
                vi <- value[[i]]
                if (is.na(vi)) {
                    stop(simpleError("unknown name: <NA>", call))
                } else {
                    stop(simpleError(sprintf("unknown name: \"%s\"", vi), call))
                }
            } else {
                n <- length(x)
                inew <- (n + 1):(n + nnew)
                vnew <- value[new]
                index[new] <- inew[match(vnew, vnew)] # handle duplicates
            }
        }
        index
    }
}


arg_record_index1 <- function(n, i, call = sys.call(-1L))
{
    if (n == 0)
        stop("missing index")

    i1 <- i[[1]]

    n1 <- length(i1)
    if (n1 != 1)
        stop(sprintf("non-scalar index (length %.0f)", n1))

    if (is.numeric(i1)) {
        i1 <- as.numeric(i1)
        if (!(is.finite(i1) && i >= 1)) {
            stop(sprintf("invalid index (%s)", i1))
        }
    } else if (is.logical(i1)) {
        stop(sprintf("invalid index (%s)", i1))
    } else {
        i1 <- as.character(i1)
    }

    i1
}
