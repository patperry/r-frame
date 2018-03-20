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


new_format_style <- function(control)
{
    utf8 <- output_utf8()
    if (output_ansi()) {
        escapes <- control$faint
        bold  <- function(x) paste0("\x1b[", control$bold, "m",
                                    utf8_encode(x, display = TRUE,
                                                utf8 = utf8),
                                    "\x1b[0m")
        faint <- function(x) paste0("\x1b[", control$faint, "m",
                                    utf8_encode(x, display = TRUE,
                                                utf8 = utf8),
                                    "\x1b[0m")
    } else {
        escapes <- NULL
        bold <- faint <- function(x)
            utf8_encode(x, display = TRUE, utf8 = utf8)
    }

    normal <- function(x, width) {
        x <- utf8_encode(x, escapes = escapes,
                         display = TRUE, utf8 = utf8)
        x[is.na(x)] <- utf8_encode("<NA>", width = width,
                                   display = TRUE, utf8 = utf8)
        x
    }

    as.record(list(normal = normal, bold = bold, faint = faint))
}


col_width <- function(name, x, limit = NA)
{
    n <- utf8_width(name)

    if (length(dim(x)) <= 1) {
        w <- max(0, utf8_width(x), na.rm = TRUE)
        if (anyNA(x)) {
            naw <- utf8_width("<NA>")
            w <- max(w, naw)
        }
    } else {
        nc <- ncol(x)
        names <- colnames(x)
        w <- 0

        for (j in seq_len(nc)) {
            if (j > 1) {
                w <- w + 1
            }

            xj <- x[, j, drop = TRUE]
            wj <- col_width(names[[j]], xj, limit - w)

            w <- w + wj
            if (isTRUE(w >= limit))
                return(limit)
        }
    }

    w <- max(n, w)
    if (isTRUE(w >= limit))
        return(limit)
    w
}


format_vector <- function(name, x, control, indent, page)
{
    num <- is.numeric(x) || is.complex(x)
    justify <- if (num) "right" else "left"

    if ((is.list(x) && !is.object(x)) || is.record(x)) {
        y <- vapply(x, format_entry, "", control, indent)
    } else {
        if (!is.character(x)) {
            y <- format(x, control = control, indent = indent,
                        justify = "none")
        } else {
            y <- x
        }

        if (!num) {
            ellipsis <- utf8_width(control$ellipsis)
            chars <- max(24, control$line - indent - ellipsis)
            y <- utf8_format(y, chars = chars, justify = "none")
            y[is.na(x)] <- "<NA>"
        }
    }

    # compute width, determine whether to truncate
    if (!is.na(control$pages) && page == control$pages) {
        limit <- control$line - indent
        width <- col_width(name, y, limit + 1)
        trunc <- (width > limit)
    } else {
        width <- col_width(name, y)
        trunc <- FALSE
    }

    # truncate if necessary
    if (trunc) {
        y <- rep(control$ellipsis, length(y))
        width <- utf8_width(control$ellipsis)
        name <- control$ellipsis
    }

    # compute new indent
    start <- (indent == 0L)
    next_indent <- indent + width + 1
    if (next_indent > control$line + 1 && !start
            && !is.na(control$pages) && page < control$pages) {
        # new page, re-format with new indent
        format_vector(name, x, control, 0L, page + 1L)
    } else {
        list(name = name, value = y, trunc = trunc,
             page = page, indent = indent, width = width,
             justify = justify, next_page = page,
             next_indent = next_indent)
    }
}


format_matrix <- function(name, x, control, indent, page)
{
    nc <- dim(x)[[2L]]
    if (nc == 0L) {
        x <- rep_len("[]", nrow(x))
        return(format_vector(name, x, control, indent, page))
    }

    names <- dimnames(x)[[2L]]
    if (is.null(names)) {
        names <- paste0("[,", as.character(seq_len(nc)), "]")
    } else {
        isna <- which(is.na(names) | !nzchar(names))
        if (length(isna) > 0) {
            names[isna] <- paste0("[,", as.character(isna), "]")
        }
    }
    y <- as.record(vector("list", nc))
    trunc <- FALSE

    ellipsis <- utf8_width(control$ellipsis)
    line <- control$line
    pages <- control$pages

    page_start <- page
    indent_start <- indent
    next_page <- page
    next_indent <- indent
    page <- vector("list", nc)
    indent <- vector("list", nc)
    width <- vector("list", nc)
    justify <- vector("list", nc)

    for (j in seq_len(nc)) {
        if (!is.na(pages) && next_page == pages && j < nc) {
            control$line <- line - 1 - ellipsis
        } else {
            control$line <- line
        }

        xj <- if (is.data.frame(x)) x[[j]] else x[, j, drop = TRUE]

        fmt <- format_column(names[[j]], xj, control, next_indent, next_page)

        names[[j]] <- fmt$name
        y[[j]] <- fmt$value
        page[[j]] <- fmt$page
        indent[[j]] <- fmt$indent
        width[[j]] <- fmt$width
        justify[[j]] <- fmt$justify
        next_page <- fmt$next_page
        next_indent <- fmt$next_indent

        # if at end add extra indent to fit name
        if (j == nc) {
            if (next_page != page_start) {
                indent_start <- 0L
            }
            next_indent <- max(next_indent,
                               indent_start + utf8_width(name) + 1)
        }

        if (fmt$trunc) {
            if (j < nc && length(dim(xj)) > 1) {
                j <- j + 1
                names[[j]] <- control$ellipsis
                y[[j]] <- rep(control$ellipsis, nrow(x))
                page[[j]] <- next_page
                indent[[j]] <- next_indent
                width[[j]] <- ellipsis
                justify[[j]] <- "left"
                next_indent <- next_indent + ellipsis + 1
            }
            y <- y[1:j]
            names <- names[1:j]
            page <- page[1:j]
            indent <- indent[1:j]
            width <- width[1:j]
            justify <- justify[1:j]
            trunc <- TRUE
            break
        }
    }

    names(y) <- names
    y <- as.dataset(y)
    list(name = name, value = y, trunc = trunc, page = page,
         indent = indent, width = width, justify = justify,
         next_page = next_page, next_indent = next_indent)
}


format_column <- function(name, x, control, indent, page)
{
    if (length(dim(x)) <= 1) {
        format_vector(name, x, control, indent, page)
    } else {
        format_matrix(name, x, control, indent, page)
    }
}


ncol_recursive <- function(x, offset = 0)
{
    if (length(dim(x)) <= 1) {
        offset + 1
    } else if (is.dataset(x)) {
        for (j in seq_len(ncol(x))) {
            offset <- ncol_recursive(x[[j]], offset)
        }
        offset
    } else {
        offset + ncol(x)
    }
}


format.dataset <- function(x, limit = NA, control = NULL, indent = 0,
                           meta = FALSE, ...)
{
    x       <- as.dataset(x)
    limit   <- as.limit(limit)
    control <- as.format.control(control)
    indent  <- as.indent(indent)
    meta    <- as.option(meta)

    rtrunc <- FALSE
    if (isTRUE(limit > 0) && (nrow(x) > limit)) {
        rtrunc <- TRUE
        x <- x[seq_len(limit), , drop = FALSE]
    }

    fmt   <- format_column("", x, control, indent, 1L)

    y <- fmt$value
    keys(y) <- keys(x)
    if (meta) {
        attr(y, "format.meta") <-
            list(trunc_rows = rtrunc,
                 trunc_cols = fmt$trunc,
                 page       = fmt$page,
                 indent     = fmt$indent,
                 width      = fmt$width,
                 justify    = fmt$justify)
    }

    y
}


print_header <- function(control, style, index, path, names, indent, width,
                         row_head, row_width)
{
    n <- length(index)

    # determine header for nested groups
    depth <- max(1, vapply(index, length, 0))
    group <- matrix(unlist(lapply(index, `length<-`, depth)), nrow = depth)
    gname <- matrix(unlist(lapply(path, `length<-`, depth)), nrow = depth)

    for (d in seq(from = depth - 1, by = -1, length.out = depth - 1)) {
        i <- 1
        while (i <= n) {
            g <- group[d, i]
            if (!is.na(g)) {
                j <- i
                while (j < n && group[d, j + 1] %in% g) {
                    j <- j + 1
                }
                if (all(is.na(group[d + 1, i:j]))) {
                    k <- d + 1
                    while (k < depth && all(is.na(group[k + 1, i:j]))) {
                        k <- k + 1
                    }
                    group[k, i:j] <- group[d, i:j]
                    group[d, i:j] <- NA
                    gname[k, i:j] <- gname[d, i:j]
                    gname[d, i:j] <- NA
                }
                i <- j + 1
            } else {
                i <- i + 1
            }
        }
    }

    # print header
    for (d in seq_len(depth - 1)) {
        grp <- group[d, ]
        gnm <- gname[d, ]
        head <- format("", width = row_width)
        pos <- 0
        i <- 1
        while (i <= n) {
            head <- paste0(head, format("", width = indent[[i]] - pos))
            pos <- indent[[i]]

            if (is.na(grp[[i]])) {
                w <- width[[i]]
                head <- paste0(head, format("", width = w))
                pos <- pos + w
            } else {
                # advance to the end of the group
                g <- grp[[i]]
                while (i < n && grp[[i + 1]] %in% g) { # use %in% to handle NA
                    i <- i + 1
                }
                w <- (indent[[i]] - pos) + width[[i]]

                # center justify group name, using banner instead of spaces
                nm <- gnm[[i]]
                wnm <- utf8_width(nm)
                pad <- max(0, w - wnm)
                lpad <- floor(pad / 2)
                rpad <- ceiling(pad / 2)
                banner <- paste0(paste0(rep(control$horiz2, lpad),
                                        collapse = ""),
                                 nm,
                                 paste0(rep(control$horiz2, rpad),
                                        collapse = ""))
                head <- paste0(head, style$bold(banner))
                pos <- pos + max(w, wnm)
            }
            i <- i + 1
        }
        cat(head, "\n", sep = "")
    }

    head <- row_head
    pos <- 0
    for (i in seq_len(n)) {
        head <- paste0(head, format("", width = indent[[i]] - pos), names[[i]])
        pos <- indent[[i]] + width[[i]]
    }

    cat(head, "\n", sep = "")
}


print_body <- function(control, cols, indent, width, row_body)
{
    n <- length(cols)
    body <- row_body
    pos <- 0
    for (i in seq_len(n)) {
        body <- paste0(body, format("", width = indent[[i]] - pos), cols[[i]])
        pos <- indent[[i]] + width[[i]]
    }
    cat(body, sep = "\n")
}


format_rows <- function(control, style, nrow, number, keys)
{
    if (number) {
        row_body <- utf8_format(as.character(seq_len(nrow)),
                                chars = .Machine$integer.max,
                                justify = "left")
        num_width <- max(0, utf8_width(row_body))
        row_head <- utf8_format("", width = num_width)
    } else {
        row_body <- rep("", nrow)
        num_width <- 0
        row_head <- ""
    }

    if (!is.null(keys)) {
        names <- names(keys)
        if (is.null(names)) {
            names(keys) <- character(length(keys))
        }
        kcontrol <- control
        kcontrol$line <- .Machine$integer.max - 1
        cols <- format.dataset(keys, control = kcontrol, meta = TRUE)
        meta <- attr(cols, "format.meta")
        width <- unlist(meta$width)
        justify <- unlist(meta$justify)

        kb <- mapply(function(k, w, j)
                         utf8_format(k, chars = .Machine$integer.max,
                                     justify = j, width = w),
                     cols, width, justify,
                     SIMPLIFY = FALSE, USE.NAMES = FALSE)
        kh <- mapply(function(n, w, j)
                        utf8_format(n, chars = .Machine$integer.max,
                                    justify = j, width = w),
                     names(cols), width, justify, USE.NAMES = FALSE)

        kb <- do.call(paste, kb)
        kh <- paste(kh, collapse = " ")
        if (nzchar(row_head)) {
            row_head <- paste(row_head, kh)
            row_body <- paste(row_body, kb)
        } else {
            row_head <- kh
            row_body <- kb
        }
    }

    row_width <- utf8_width(row_head)
    row_head <- style$faint(row_head)
    row_body <- style$faint(row_body)

    if (!is.null(keys)) {
        row_head <- paste0(row_head, " ", style$faint(control$vline), " ")
        row_width <- row_width + 3
        row_body <- paste0(row_body, " ", style$faint(control$vline), " ")
    } else if (row_width > 0) {
        row_head <- paste0(row_head, " ")
        row_body <- paste0(row_body, " ")
        row_width <- row_width + 1
    }

    list(width = row_width, head = row_head, body = row_body)
}


print.dataset <- function(x, limit = NULL, control = NULL, ...)
{
    x       <- as.dataset(x)
    limit   <- as.limit(limit)
    control <- as.format.control(control)

    number  <- is.null(keys(x))
    n <- nr <- dim(x)[[1L]]
    style <- new_format_style(control)

    if (length(x) == 0) {
        cat(sprintf("(%.0f rows, 0 columns)\n", n))
        return(invisible(x))
    }

    if (!is.na(limit) && limit >= 0) {
        n <- min(n, limit)
    }

    keys <- keys(x)[seq_len(n), , drop = FALSE]
    rfmt <- format_rows(control = control, style = style, nrow = n,
                       number = number, keys = keys)
    row_width <- rfmt$width
    row_head <- rfmt$head
    row_body <- rfmt$body

    control$line <- max(1L, control$line - row_width)
    fmt <- format.dataset(x, limit = limit, control = control, meta = TRUE)
    meta <- attr(fmt, "format.meta")
    page <- unlist(meta$page)
    indent <- unlist(meta$indent)
    width <- unlist(meta$width)
    justify <- unlist(meta$justify)

    cols <- flatten_dataset(fmt, flat = TRUE, path = TRUE)
    path <- attr(cols, "path")
    index <- attr(cols, "index")
    names <- vapply(path, tail, "", n = 1)

    # justify columns, names
    cols <- mapply(function(col, w, j)
                       utf8_format(as.character(col), width = w,
                                   chars = .Machine$integer.max,
                                   justify = j),
                   cols, width, justify, SIMPLIFY = FALSE, USE.NAMES = FALSE)
    names <- mapply(function(name, w, j)
                        utf8_format(name, width = w,
                                    chars = .Machine$integer.max, justify = j),
                    names, width, justify,
                    SIMPLIFY = TRUE, USE.NAMES = FALSE)

    # apply formatting
    cols <- mapply(style$normal, cols, width,
                   SIMPLIFY = FALSE, USE.NAMES = FALSE)
    names <- style$bold(names)

    foot_width <- row_width
    start <- 1L
    pg <- 1L
    for (i in seq_along(cols)) {
        if (i < length(cols) && page[[i + 1L]] == pg) {
            next
        }

        if (start > 1L) {
            cat(style$faint(control$vellipsis), "\n", sep="")
        }

        print_header(control = control, style = style,
                     index = index[start:i], path = path[start:i],
                     names = names[start:i], indent = indent[start:i],
                     width = width[start:i], row_head = row_head,
                     row_width = row_width)

        if (n > 0) {
            print_body(control = control, cols = cols[start:i],
                       indent = indent[start:i], width = width[start:i],
                       row_body = row_body)
        }

        foot_width <- max(foot_width, row_width + indent[[i]] + width[[i]])
        start <- i + 1L
        pg <- pg + 1L
    }

    if (meta$trunc_cols) {
        nc <- ncol_recursive(x)

        if (meta$trunc_rows) {
            caption <- sprintf("(%.0f rows, %.0f columns total)", n, nc)
        } else {
            caption <- sprintf("(%.0f columns total)", nc)
        }
    } else if (meta$trunc_rows) {
        caption <- sprintf("(%.0f rows total)", n)
    } else {
        caption <- NULL
    }

    if (nr == 0) {
        cat("(0 rows)\n")
    } else if (!is.null(caption)) {
        vellipsis <- if (meta$trunc_rows) control$vellipsis else ""
        foot <- utf8_format(paste0(" ", caption),
                            width = max(0,
                                        foot_width
                                        - utf8_width(vellipsis)),
                            justify = "right")
        cat(style$faint(vellipsis), foot, "\n", sep="")
    }

    invisible(x)
}
