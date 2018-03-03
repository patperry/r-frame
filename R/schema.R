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

schema <- function(x)
{
    UseMethod("schema")
}


schema.default <- function(x)
{
    if (is.null(x))
        return(NULL)

    d <- dim(x)
    r <- length(d)
    if (r <= 1) {
        x[0]
    } else if (r == 2) {
        x[0, , drop = TRUE]
    } else {
        stop(sprintf("cannot get schema of rank-%.0f object", r))
    }
}


schema.record <- function(x)
{
    x <- lapply(x, schema)
    as.record(x)
}
