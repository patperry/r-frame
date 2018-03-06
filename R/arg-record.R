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
