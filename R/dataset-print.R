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

    normal <- function(x) {
        utf8_encode(x, escapes = escapes, display = TRUE, utf8 = utf8)
    }

    as.record(list(normal = normal, bold = bold, faint = faint))
}


format_vector <- function(index, name, x, control, indent, page)
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
            if (is.na(control$line)) {
                chars <- .Machine$integer.max
            } else {
                chars <- max(24, control$line - indent - ellipsis)
            }
            y <- utf8_format(y, chars = chars, justify = "none")
            y[is.na(x)] <- "<NA>"
        } else {
            y[is.na(x)] <- "NA"
        }
    }

    # compute width, determine whether to truncate
    width <- max(utf8_width(name), utf8_width(y))
    if (isTRUE(page == control$pages) && !is.na(control$line)) {
        limit <- control$line - indent
        trunc <- (width > limit)
    } else {
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
    if (isTRUE(next_indent > control$line + 1) && !start
            && isTRUE(page < control$pages)) {
        # new page, re-format with new indent
        format_vector(index, name, x, control, 0L, page + 1L)
    } else {
        list(name = name, value = y, trunc = trunc,
             index = list(index),
             page = page, indent = indent, width = width,
             justify = justify, next_page = page,
             next_indent = next_indent)
    }
}


fix_col_names <- function(names, n)
{
    if (is.null(names)) {
        names <- paste0("[,", seq_len(n), "]")
    } else {
        isna <- which(is.na(names) | !nzchar(names))
        if (length(isna) > 0) {
            names[isna] <- paste0("[,", isna, "]")
        }
    }
    names
}


format_matrix <- function(index, name, x, control, indent, page)
{
    nc <- dim(x)[[2]]
    if (nc == 0) {
        x <- rep_len("[]", dim(x)[[1]])
        return(format_vector(index, name, x, control, indent, page))
    }

    names <- fix_col_names(dimnames(x)[[2]], nc)
    y <- as.record(vector("list", nc))
    trunc <- FALSE

    ellipsis <- utf8_width(control$ellipsis)
    line  <- control$line
    pages <- control$pages

    start_page   <- page
    start_indent <- indent
    next_page <- page
    next_indent <- indent
    colindex <- vector("list", nc)
    page     <- vector("list", nc)
    indent   <- vector("list", nc)
    width    <- vector("list", nc)
    justify  <- vector("list", nc)

    for (j in seq_len(nc)) {
        if (isTRUE(next_page == pages) && j < nc) {
            control$line <- line - 1 - ellipsis
        } else {
            control$line <- line
        }

        xj  <- x[, j, drop = TRUE]
        if (is.null(xj))
            xj <- vector("list", nrow(x))

        fmt <- format_column(c(index, j), names[[j]], xj, control,
                             next_indent, next_page)

        names[[j]]    <- fmt$name
        y[[j]]        <- fmt$value
        colindex[[j]] <- fmt$index
        page[[j]]     <- fmt$page
        indent[[j]]   <- fmt$indent
        width[[j]]    <- fmt$width
        justify[[j]]  <- fmt$justify

        next_page <- fmt$next_page
        next_indent <- fmt$next_indent

        # if at end add extra indent to fit name
        if (j == nc) {
            if (next_page != start_page) {
                start_indent <- 0L
            }
            next_indent <- max(next_indent,
                               start_indent + utf8_width(name) + 1)
        }

        if (fmt$trunc) {
            if (j < nc && length(dim(xj)) > 1) {
                j <- j + 1L
                names[[j]]    <- control$ellipsis
                y[[j]]        <- rep(control$ellipsis, nrow(x))
                colindex[[j]] <- list(c(index, j))
                page[[j]]     <- next_page
                indent[[j]]   <- next_indent
                width[[j]]    <- ellipsis
                justify[[j]]  <- "left"
                next_indent <- next_indent + ellipsis + 1L
            }

            y <- y[1:j]
            names <- names[1:j]
            colindex <- colindex[1:j]
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
    list(name    = name,
         value   = y,
         index   = do.call(c, colindex),
         page    = do.call(c, page),
         indent  = do.call(c, indent),
         width   = do.call(c, width),
         justify = do.call(c, justify),
         trunc       = trunc,
         next_page   = next_page,
         next_indent = next_indent)
}


format_column <- function(index, name, x, control, indent, page)
{
    if (length(dim(x)) <= 1) {
        format_vector(index, name, x, control, indent, page)
    } else {
        format_matrix(index, name, x, control, indent, page)
    }
}


ncol_recursive <- function(x, offset = 0)
{
    d <- dim(x)
    if (length(d) <= 1) {
        offset + 1
    } else if (is.dataset(x) || is.data.frame(x)) {
        for (j in seq_len(d[[2]])) {
            offset <- ncol_recursive(x[[j]], offset)
        }
        offset
    } else {
        offset + d[[2]]
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

    fmt <- format_column(integer(), "", x, control, indent, 1)

    y       <- fmt$value
    keys(y) <- keys(x)

    if (meta) {
        meta <- dataset(index   = fmt$index,
                        page    = fmt$page,
                        indent  = fmt$indent,
                        width   = fmt$width,
                        justify = fmt$justify)
        attr(y, "format.meta") <- meta
        attr(y, "format.meta.rows.trunc") <- rtrunc
        attr(y, "format.meta.cols.trunc") <- fmt$trunc
    }

    y
}


format_head <- function(x, meta, style, char)
{
    # find column names and paths to nested columns
    n <- nrow(meta)
    path <- vector("list", n)
    names <- character(n)

    for (i in seq_len(n)) {
        index <- meta$index[[i]]
        m <- length(index)
        p <- character(m)
        y <- x
        for (j in seq_len(m)) {
            k <- index[[j]]
            p[[j]] <- names(y)[[k]]
            if (j < m) {
                y <- y[[k]]
            }
        }
        path[[i]]  <- p
        names[[i]] <- p[[m]]
    }

    # determine header for nested groups
    depth <- max(1, vapply(meta$index, length, 0))
    group <- matrix(unlist(lapply(meta$index, `length<-`, depth)), nrow = depth)
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

    # format group header
    lines <- character(depth)

    for (d in seq_len(depth - 1)) {
        grp <- group[d, ]
        gnm <- gname[d, ]
        head <- ""
        pos <- 0
        i <- 1
        while (i <= n) {
            head <- paste0(head, format("", width = meta$indent[[i]] - pos))
            pos <- meta$indent[[i]]

            if (is.na(grp[[i]])) {
                w <- meta$width[[i]]
                head <- paste0(head, format("", width = w))
                pos <- pos + w
            } else {
                # advance to the end of the group
                g <- grp[[i]]
                while (i < n && grp[[i + 1]] %in% g) { # use %in% to handle NA
                    i <- i + 1
                }
                w <- (meta$indent[[i]] - pos) + meta$width[[i]]

                # center justify group name, using banner instead of spaces
                nm <- gnm[[i]]
                wnm <- utf8_width(nm)
                pad <- max(0, w - wnm)
                lpad <- floor(pad / 2)
                rpad <- ceiling(pad / 2)
                banner <- paste0(paste0(rep(char, lpad),
                                        collapse = ""),
                                 nm,
                                 paste0(rep(char, rpad),
                                        collapse = ""))
                head <- paste0(head, style(banner))
                pos <- pos + max(w, wnm)
            }
            i <- i + 1
        }

        lines[[d]] <- head
    }

    # format names
    for (i in seq_len(n)) {
        names[[i]] <- utf8_format(names[[i]], width = meta$width[[i]],
                                  justify = meta$justify[[i]],
                                  chars = .Machine$integer.max)
    }
    names <- style(names)

    head <- ""
    pos <- 0
    for (i in seq_len(n)) {
        gap <- format("", width = meta$indent[[i]] - pos)
        head <- paste0(head, style(gap), names[[i]])
        pos <- meta$indent[[i]] + meta$width[[i]]
    }

    lines[[depth]] <- head
    attr(lines, "width") <- pos
    lines
}


print_head <- function(row, x, meta, control, style)
{
    lines <- format_head(x, meta, style, control$horiz2)
    depth <- length(lines)

    row_head  <- row$head
    row_depth <- length(row_head)

    if (row_depth < depth) {
        pad <- format("", width = row$width)
        row_head <- c(rep_len(pad, depth - row_depth), row_head)
    } else if (row_depth > depth) {
        pad <- format("", width = attr(lines, "width", TRUE))
        lines <- c(rep_len(pad, row_depth - depth), lines)
    }

    lines <- paste0(row_head, lines)
    if (length(lines) > 0)
        cat(lines, sep = "\n")
}


format_body <- function(x, meta, style)
{
    n <- nrow(meta)
    cols <- vector("list", n)
    for (i in seq_len(n)) {
        y <- x[[meta$index[[i]]]]
        y <- utf8_format(y, width = meta$width[[i]],
                         justify = meta$justify[[i]],
                         chars = .Machine$integer.max)
        y <- style(y)
        cols[[i]] <- y
    }

    lines <- character(nrow(x))
    if (length(lines) > 0) {
        pos <- 0
        for (i in seq_len(n)) {
            gap <- format("", width = meta$indent[[i]] - pos)
            lines <- paste0(lines, style(gap), cols[[i]])
            pos <- meta$indent[[i]] + meta$width[[i]]
        }
    }

    attr(lines, "width") <- (meta$indent[[n]] + meta$width[[n]])
    lines
}


print_body <- function(row, x, meta, control, style)
{
    lines <- format_body(x, meta, style)
    if (length(lines) > 0) {
        lines <- paste0(row$body, lines)
        cat(lines, sep = "\n")
    }
}


format_rows <- function(nrow, keys, control, style)
{
    if (is.null(keys)) {
        body  <- format(as.character(seq_len(nrow)), justify = "left")
        width <- max(0, utf8_width(body))
        head  <- format("", width = width)

        head <- style(head)
        body <- style(body)
    } else {
        control$line <- NA
        keys <- format.dataset(keys, nrow, control, meta = TRUE)
        meta <- attr(keys, "format.meta")

        key_head  <- format_head(keys, meta, style, control$horiz2)
        key_body  <- format_body(keys, meta, style)
        key_width <- attr(key_head, "width")

        gap  <- style(" ")
        head  <- paste0(key_head, gap, style(control$vline))
        body  <- paste0(key_body, gap, style(control$vline))
        width <- key_width + 1 + utf8_width(control$vline)
    }

    list(width = width, head = head, body = body)
}


print.dataset <- function(x, limit = NULL, control = NULL, ...)
{
    x       <- as.dataset(x)
    limit   <- as.limit(limit)
    control <- as.format.control(control)

    n <- nr <- dim(x)[[1L]]
    style <- new_format_style(control)

    if (length(x) == 0) {
        cat(sprintf("(%.0f rows, 0 columns)\n", n))
        return(invisible(x))
    }

    if (isTRUE(limit >= 0)) {
        n <- min(n, limit)
    }

    row <- format_rows(n, keys(x), control, style$faint)
    if (row$width > 0) {
        row$head  <- paste0(row$head, style$normal(" "))
        row$body  <- paste0(row$body, style$normal(" "))
        row$width <- row$width + 1
    }

    if (!is.na(control$line)) {
        control$line <- max(1L, control$line - row$width)
    }

    fmt   <- format.dataset(x, limit, control, meta = TRUE)
    meta  <- attr(fmt, "format.meta", TRUE)
    npage <- max(0, meta$page)

    for (page in seq_len(npage)) {
        if (page > 1) {
            cat(style$faint(control$vellipsis), "\n", sep = "")
        }

        mp <- meta[meta$page == page, ]
        print_head(row, fmt, mp, control, style$bold)
        print_body(row, fmt, mp, control, style$normal)
    }

    rows.trunc <- attr(fmt, "format.meta.rows.trunc", TRUE)
    cols.trunc <- attr(fmt, "format.meta.cols.trunc", TRUE)
    if (cols.trunc) {
        nc <- ncol_recursive(x)
        if (rows.trunc) {
            caption <- sprintf("(%.0f rows, %.0f columns total)", n, nc)
        } else {
            caption <- sprintf("(%.0f columns total)", nc)
        }
    } else if (rows.trunc) {
        caption <- sprintf("(%.0f rows total)", n)
    } else {
        caption <- NULL
    }

    if (nr == 0) {
        cat("(0 rows)\n")
    } else if (!is.null(caption)) {
        vellipsis <- if (rows.trunc) control$vellipsis else ""
        width     <- row$width + max(meta$indent + meta$width)
        foot_width <- max(0, width - utf8_width(vellipsis))
        foot <- utf8_format(paste0(" ", caption), width = foot_width,
                            justify = "right")
        cat(style$faint(vellipsis), foot, "\n", sep = "")
    }

    invisible(x)
}
