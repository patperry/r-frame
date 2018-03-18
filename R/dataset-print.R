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


new_format_control <- function(chars = NULL, digits = NULL,
                               na.encode = TRUE, quote = FALSE,
                               na.print = NULL, print.gap = NULL,
                               justify = "none", width = NULL,
                               display = TRUE, line = NULL, pages = NULL)
{
    control <- list()
    control$chars <- chars
    control$digits <- digits
    control$na.encode <- na.encode
    control$quote <- quote
    control$na.print <- na.print
    control$print.gap <- print.gap
    control$justify <- justify
    control$width <- width
    control$display <- display
    control$line <- line
    control$pages <- pages
    control$ansi <- output_ansi()
    control$utf8 <- output_utf8()

    if (is.null(control$na.print)) {
        control$na.print <- if (control$quote) "NA" else "<NA>"
    }
    if (is.null(control$print.gap)) {
        control$print.gap <- 1L
    }
    if (is.null(control$width)) {
        control$width <- 0L
    }
    if (is.null(control$line)) {
        control$line <- getOption("width")
    }

    control$banner    <- utf8_fallback("\u2550", "=")
    control$ellipsis  <- utf8_fallback("\u2026", "...")
    control$times     <- utf8_fallback("\u00d7", "x")
    control$vellipsis <- utf8_fallback("\u22ee", ".")
    control$vline     <- utf8_fallback("\u2502", "|")

    control
}


new_format_style <- function(control)
{
    if (control$ansi) {
        escapes <- style_faint
        bold  <- function(x) paste0("\x1b[", style_bold, "m",
                                    utf8_encode(x, display = control$display,
                                                utf8 = control$utf8),
                                    "\x1b[0m")
        faint <- function(x) paste0("\x1b[", style_faint, "m",
                                    utf8_encode(x, display = control$display,
                                                utf8 = control$utf8),
                                    "\x1b[0m")
    } else {
        escapes <- NULL
        bold <- faint <- function(x)
            utf8_encode(x, display = control$display, utf8 = control$utf8)
    }

    normal <- function(x, width) {
        x <- utf8_encode(x, quote = control$quote, escapes = escapes,
                         display = control$display, utf8 = control$utf8)
        x[is.na(x)] <- utf8_encode(control$na.print, width = width,
                                   display = control$display,
                                   utf8 = control$utf8)
        x
    }

    list(normal = normal, bold = bold, faint = faint)
}


col_width <- function(name, x, control, limit = NULL)
{
    limit <- if (is.null(limit)) Inf else limit
    n <- utf8_width(name)
    ellipsis <- utf8_width(control$ellipsis)
    gap <- control$print.gap

    if (length(dim(x)) <= 1) {
        w <- max(0, utf8_width(x, quote = control$quote), na.rm = TRUE)
        if (anyNA(x)) {
            naw <- utf8_width(control$na.print)
            w <- max(w, naw)
        }
    } else {
        nc <- ncol(x)
        names <- colnames(x)
        w <- 0

        for (j in seq_len(nc)) {
            if (j > 1) {
                w <- w + gap
            }

            xj <- if (is.data.frame(x)) x[[j]] else x[, j, drop = TRUE]
            wj <- col_width(names[[j]], xj, control, limit - w)

            w <- w + wj
            if (w >= limit) {
                return(limit)
            }
        }
    }

    w <- max(n, w)
    min(limit, w)
}

format_list <- function(x, width, control)
{
    times <- control$times
    y <- vapply(x, FUN.VALUE = "", function(elt) class(elt)[[1L]])
    suffix <- vapply(x, FUN.VALUE = "", function(elt) {
        d <- dim(elt)
        n <- length(elt)
        if (!is.null(d)) {
            paste0("[", paste0(d, collapse = ", "), "]")
        } else if (!is.null(n) && !is.null(elt)) {
            n <- length(elt)
            paste0("(", n, ")")
        } else {
            ""
        }
    })
    y <- paste0(y, suffix)
    utf8_format(y, justify = control$justify, width = width,
                quote = control$quote)
}

format_vector <- function(name, x, ..., control, section, indent)
{
    chars <- control$chars
    gap <- control$print.gap
    ellipsis <- utf8_width(control$ellipsis)
    if ((stretch <- is.null(chars))) {
        quotes <- if (control$quote) 2 else 0
        chars <- max(24, control$line - indent - ellipsis - quotes)
    }

    # determine column justification
    justify <- if (is.numeric(x) || is.complex(x)) "right" else "left"

    # determine the minimum element width
    min_width <- max(control$width, utf8_width(name))

    # convert factor to character
    cl <- class(x)
    if (is.factor(x)) {
        x <- as.character(x)
        cl <- class(x)
    }

    # format character and list specially, otherwise use S3
    if (is.character(x) && (identical(cl, "character")
                            || identical(cl, "AsIs"))) {
        y <- utf8_format(x, chars = chars, justify = control$justify,
                         width = min_width, na.encode = control$na.encode,
                         quote = control$quote, na.print = control$na.print)
    } else if (is.list(x) && identical(cl, "list")) {
        y <- format_list(x, min_width, control)
    } else {
        y <- format(x, ..., chars = chars, na.encode = control$na.encode,
                    quote = control$quote, na.print = control$na.print,
                    print.gap = control$print.gap, justify = control$justify,
                    width = min_width)
    }

    # compute width, determine whether to truncate
    if (!is.na(control$pages) && section - 1L == control$pages) {
        limit <- control$line - indent
        width <- col_width(name, y, control, limit + 1)
        trunc <- (width > limit)
    } else {
        width <- col_width(name, y, control)
        trunc <- FALSE
    }

    # truncate if necessary
    if (trunc) {
        y <- rep(control$ellipsis, length(y))
        width <- utf8_width(control$ellipsis)
        name <- control$ellipsis
        right <- FALSE
    }

    # compute new indent
    start <- (indent == 0L)
    next_indent <- indent + width + gap
    if (next_indent > control$line + gap && !start
            && !is.na(control$pages) && section - 1L < control$pages) {
        # new page, re-format with new indent
        format_vector(name, x, ..., control = control,
                      section = section + 1L, indent = 0L)
    } else {
        list(name = name, value = y, trunc = trunc,
             section = section, indent = indent, width = width,
             justify = justify, next_section = section,
             next_indent = next_indent)
    }
}


format_matrix <- function(name, x, ..., control, section, indent)
{
    nc <- dim(x)[[2L]]
    if (nc == 0L) {
        x <- flatten_dataset(record(x), flat = TRUE)[[1L]]
        return(format_vector(name, x, ..., control = control,
                             section = section, indent = indent))
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

    gap <- control$print.gap
    ellipsis <- utf8_width(control$ellipsis)
    line <- control$line
    pages <- control$pages

    section_start <- section
    indent_start <- indent
    next_section <- section
    next_indent <- indent
    section <- vector("list", nc)
    indent <- vector("list", nc)
    width <- vector("list", nc)
    justify <- vector("list", nc)

    for (j in seq_len(nc)) {
        if (!is.na(pages) && next_section - 1L == pages && j < nc) {
            control$line <- line - gap - ellipsis
        } else {
            control$line <- line
        }

        xj <- if (is.data.frame(x)) x[[j]] else x[, j, drop = TRUE]

        fmt <- format_column(names[[j]], xj, ..., control = control,
                             section = next_section, indent = next_indent)

        names[[j]] <- fmt$name
        y[[j]] <- fmt$value
        section[[j]] <- fmt$section
        indent[[j]] <- fmt$indent
        width[[j]] <- fmt$width
        justify[[j]] <- fmt$justify
        next_section <- fmt$next_section
        next_indent <- fmt$next_indent

        # if at end add extra indent to fit name
        if (j == nc) {
            if (next_section != section_start) {
                indent_start <- 0L
            }
            next_indent <- max(next_indent,
                               indent_start + utf8_width(name) + gap)
        }

        if (fmt$trunc) {
            if (j < nc && length(dim(xj)) > 1) {
                j <- j + 1
                names[[j]] <- control$ellipsis
                y[[j]] <- rep(control$ellipsis, nrow(x))
                section[[j]] <- next_section
                indent[[j]] <- next_indent
                width[[j]] <- ellipsis
                justify[[j]] <- "left"
                next_indent <- next_indent + ellipsis + gap
            }
            y <- y[1:j]
            names <- names[1:j]
            section <- section[1:j]
            indent <- indent[1:j]
            width <- width[1:j]
            justify <- justify[1:j]
            trunc <- TRUE
            break
        }
    }

    names(y) <- names
    y <- as.dataset(y)
    list(name = name, value = y, trunc = trunc, section = section,
         indent = indent, width = width, justify = justify,
         next_section = next_section, next_indent = next_indent)
}


format_column <- function(name, x, ..., control, section, indent)
{
    vec <- length(dim(x)) <= 1
    if (vec) {
        format_vector(name, x, ..., control = control,
                      section = section, indent = indent)
    } else {
        format_matrix(name, x, ..., control = control,
                      section = section, indent = indent)
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


format.dataset <- function(x, limit = NA, pages = NA, ...,
                           indent = NULL, line = NULL, meta = FALSE)
{
    x     <- as.dataset(x)
    limit <- as.limit(limit)
    pages <- as.pages(pages)

    chars <- NULL
    na.encode <- TRUE
    quote <- FALSE
    na.print <- NULL
    print.gap <- NULL
    justify <- "none"
    width <- NULL
    indent <- if (is.null(indent)) NULL else as.integer.scalar(indent)
    line <- if (is.null(line)) NULL else as.integer.scalar(line)
    meta <- as.option(meta)

    control <- new_format_control(chars = chars, na.encode = na.encode,
                                  quote = quote, na.print = na.print,
                                  print.gap = print.gap, justify = justify,
                                  width = width, line = line, pages = pages)
    n <- dim(x)[[1L]]

    if (is.null(indent)) {
        indent <- 0L
    }
    if (is.na(limit) || limit < 0) {
        limit <- n
    }

    if ((rtrunc <- (n > limit))) {
        x <- x[seq_len(limit), , drop = FALSE]
    }

    fmt <- format_column("", x, control = control,
                         section = 1L, indent = indent)
    y <- fmt$value
    keys(y) <- keys(x)

    if ((ctrunc <- fmt$trunc)) {
        nc <- ncol_recursive(x)
    }

    if (meta) {
        if (rtrunc && ctrunc) {
            caption <- sprintf("(%.0f rows, %.0f columns total)", n, nc)
        } else if (rtrunc) {
            caption <- sprintf("(%.0f rows total)", n)
        } else if (ctrunc) {
            caption <- sprintf("(%.0f columns total)", nc)
        } else {
            caption <- NULL
        }

        attr(y, "trunc_rows") <- rtrunc
        attr(y, "trunc_cols") <- ctrunc
        attr(y, "section") <- fmt$section
        attr(y, "indent") <- fmt$indent
        attr(y, "width") <- fmt$width
        attr(y, "justify") <- fmt$justify
        attr(y, "caption") <- caption
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
                banner <- paste0(paste0(rep(control$banner, lpad),
                                        collapse = ""),
                                 nm,
                                 paste0(rep(control$banner, rpad),
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
    gap <- utf8_format("", width = control$print.gap)

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
        cols <- format.dataset(keys, chars = .Machine$integer.max,
                               na.encode = FALSE, na.print = control$na.print,
                               quote = control$quote,
                               print.gap = control$print.gap,
                               digits = control$digits,
                               line = .Machine$integer.max - control$print.gap,
                               meta = TRUE)
        width <- unlist(attr(cols, "width"))
        justify <- unlist(attr(cols, "justify"))

        kb <- mapply(function(k, w, j)
                         utf8_format(k, chars = .Machine$integer.max,
                                     justify = j, width = w,
                                     na.print = control$na.print),
                     cols, width, justify,
                     SIMPLIFY = FALSE, USE.NAMES = FALSE)
        kh <- mapply(function(n, w, j)
                        utf8_format(n, chars = .Machine$integer.max,
                                    justify = j, width = w,
                                    na.print = control$na.print),
                     names(cols), width, justify, USE.NAMES = FALSE)

        kb <- do.call(paste, c(kb, sep = gap))
        kh <- paste(kh, collapse = gap)
        if (nzchar(row_head)) {
            row_head <- paste(row_head, kh, sep = gap)
            row_body <- paste(row_body, kb, sep = gap)
        } else {
            row_head <- kh
            row_body <- kb
        }
    }

    row_width <- utf8_width(row_head)
    row_head <- style$faint(row_head)
    row_body <- style$faint(row_body)

    if (!is.null(keys)) {
        row_head <- paste0(row_head, gap, style$faint(control$vline), gap)
        row_width <- row_width + 1 + 2 * utf8_width(gap)
        row_body <- paste0(row_body, gap, style$faint(control$vline), gap)
    } else if (row_width > 0) {
        row_head <- paste0(row_head, gap)
        row_body <- paste0(row_body, gap)
        row_width <- row_width + utf8_width(gap)
    }

    list(width = row_width, head = row_head, body = row_body)
}


print.dataset <- function(x, limit = NULL, pages = NULL, ...)
{
    x     <- as.dataset(x)
    limit <- as.limit(limit)
    pages <- as.pages(pages)
    number <- is.null(keys(x))

    chars  <- NULL
    digits <- NULL
    quote <- FALSE
    na.print  <- NULL
    print.gap <- NULL
    display <- TRUE
    control <- new_format_control(chars = chars, digits = digits,
                                  quote = quote, na.print = na.print,
                                  print.gap = print.gap, display = display,
                                  pages = pages)

    n <- dim(x)[[1L]]
    style <- new_format_style(control)
    gap <- utf8_format("", width = control$print.gap)

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

    line <- max(1L, control$line - row_width)
    fmt <- format.dataset(x, limit = limit, pages = pages,
                          chars = control$chars,
                          na.encode = FALSE, na.print = control$na.print,
                          quote = control$quote, print.gap = control$print.gap,
                          digits = control$digits, line = line, meta = TRUE)
    section <- unlist(attr(fmt, "section"))
    indent <- unlist(attr(fmt, "indent"))
    width <- unlist(attr(fmt, "width"))
    justify <- unlist(attr(fmt, "justify"))

    cols <- flatten_dataset(fmt, flat = TRUE, path = TRUE)
    path <- attr(cols, "path")
    index <- attr(cols, "index")
    names <- vapply(path, tail, "", n = 1)

    # justify columns, names
    cols <- mapply(function(col, w, j)
                       utf8_format(as.character(col), width = w,
                                   chars = .Machine$integer.max,
                                   na.encode = FALSE, na.print = na.print,
                                   quote = quote, justify = j),
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
    sec <- 1L
    for (i in seq_along(cols)) {
        if (i < length(cols) && section[[i + 1L]] == sec) {
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
        sec <- sec + 1L
    }

    caption <- attr(fmt, "caption")
    if (nrow(x) == 0) {
        cat("(0 rows)\n")
    } else if (!is.null(caption)) {
        rtrunc <- attr(fmt, "trunc_rows")
        vellipsis <- if (rtrunc) control$vellipsis else ""
        foot <- utf8_format(paste0(" ", caption),
                            width = max(0,
                                        foot_width
                                        - utf8_width(vellipsis)),
                            justify = "right")
        cat(style$faint(vellipsis), foot, "\n", sep="")
    }

    invisible(x)
}
