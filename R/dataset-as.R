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


as.data.frame.dataset <- function(x, row.names = NULL, optional = FALSE, ...)
{
    x        <- as.dataset(x)
    optional <- as.option(optional)

    n     <- length(x)
    nrow  <- dim(x)[[1]]
    x     <- as.list(x)
    names <- names(x)

    attributes(x) <- NULL

    if (is.null(names)) {
        names <- character(n)
    }
    if (optional) {
        names <- make.unique(names)
    } else {
        names <- make.names(names, unique = TRUE)
    }

    names(x) <- names
    attr(x, "row.names") <- .set_row_names(nrow)
    class(x) <- "data.frame"

    as.data.frame(x, row.names)
}


as.matrix.dataset <- function(x, ...)
{
    x     <- as.dataset(x)
    n     <- length(x)
    names <- names(x)
    nrow  <- nrow(x)

    dst <- vector("list", nrow * n)
    off <- 0
    ind <- seq_len(nrow)

    for (i in seq_along(x)) {
        col <- x[[i]]

        if (is.null(col)) {
            # pass
        } else if (length(dim(col)) <= 1) {
            dst[off + ind] <- as.list(col)
        } else {
            for (j in seq_len(nrow)) {
                dst[[off + j]] <- col[j, , drop = TRUE]
            }
        }

        off <- off + nrow
    }

    x <- matrix(dst, nrow, n)
    colnames(x) <- names
    x
}


t.dataset <- function(x, ...)
{
    x <- as.dataset(x)
    x <- as.matrix(x)
    t(x)
}
