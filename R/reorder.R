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
