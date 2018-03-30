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


reorder.dataset <- function(x, ..., env = NULL)
{
    x     <- as.dataset(x)
    exprs <- substitute(list(...))
    env   <- if (is.null(env)) parent.frame() else as.environment(env)

    i <- eval.parent(call("scope", x, exprs, env))
    if (length(i) == 0) {
        return(x)
    }

    for (j in seq_along(i)) {
        i[[j]] <- xtfrm.dataset(i[[j]])
    }

    names <- names(i)
    if (!is.null(names)) {
        if (!all(names %in% c("asc", "desc", ""))) {
            stop("named arguments must be 'asc' or 'desc'")
        }
        desc <- (names == "desc")
        names(i) <- NULL
    } else {
        desc <- FALSE
    }

    i[["decreasing"]] <- desc
    i[["method"]] <- "radix"
    o <- do.call(order, i)

    x[o,]
}


sort.dataset <- function(x, decreasing = FALSE, ...)
{
    x <- as.dataset(x)
    decreasing <- as.option(decreasing)
    y <- xtfrm(x)
    o <- order(y, decreasing = decreasing)
    x[o, ]
}


xtfrm.dataset <- function(x)
{
    x <- as.dataset(x)
    x <- as.simple(x)

    n <- length(x)
    names(x) <- NULL
    null <- logical(n)

    for (i in seq_len(n)) {
        null[[i]] <- is.null(x[[i]])
        if (null[[i]])
            next
        x[[i]] <- xtfrm(x[[i]])
    }

    nx <- nrow(x)
    x <- x[!null]

    if (length(x) == 0) {
        return(seq_len(nx))
    } else if (length(x) == 1) {
        return(x[[1]])
    }

    u <- unique(x)
    o <- do.call(order, u)

    nu <- nrow(u)
    ru <- integer(nu)
    ru[o] <- seq_len(nu)

    if (nu == nx)
        ru
    else
        ru[lookup(x, u)]
}
