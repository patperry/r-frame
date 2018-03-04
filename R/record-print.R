
format_record_names <- function(x, nest = 0, indent = 2)
{
    n     <- length(x)
    names <- names(x)

    if (is.null(names)) {
        if (n > 0) {
            names <- paste0("[[", seq_len(n), "]]")
            names(x) <- names
        }
    } else {
        empty <- is.na(names) | !nzchar(names)
        if (any(empty)) {
            names[empty] <- paste0("[[", which(empty), "]]")
            names(x) <- names
        }
    }

    width <- max(0, utf8_width(names))
    if (n > 0) {
        width <- nest * indent + width
    }

    for (i in seq_len(n)) {
        xi <- x[[i]]
        if (is.record(xi)) {
            fmt <- format_record_names(xi, nest + 1, indent)
            width <- max(width, fmt$width)
            x[[i]] <- fmt$object
        }
    }

    list(object = x, width = width)
}


record_total <- function(x)
{
    n <- length(x)
    tot <- n
    for (i in seq_len(n)) {
        xi <- x[[i]]
        if (is.record(xi)) {
            tot <- tot + record_total(xi)
        }
    }
    tot
}


format_record_limit <- function(x, limit = NA)
{
    if (is.na(limit))
        return(list(object = x, trunc = FALSE, limit = limit))

    n <- length(x)
    trunc <- FALSE

    for (i in seq_len(n)) {
        if (limit == 0) {
            length(x) <- max(1, i - 1)
            trunc <- TRUE
            break
        }

        limit <- limit - 1

        xi <- x[[i]]
        if (is.record(xi)) {
            fmt <- format_record_limit(xi, limit)
            x[[i]] <- fmt$object
            trunc <- fmt$trunc
            limit <- fmt$limit
            if (trunc) {
                length(x) <- max(1, i)
                break
            }
        }
    }

    list(object = x, trunc = trunc, limit = limit)
}


format_record_values <- function(x, width)
{
    as.record(lapply(x, format_record_value, width = width))
}


format_record_value <- function(x, width)
{
    if (is.record(x)) {
        if (length(x) == 0)
            return("{}")
        else
            return(format_record_values(x, width))
    }

    cl <- paste(class(x), collapse = ".")
    d <- dim(x)
    n <- length(x)

    if (is.null(x)) {
        "NULL"
    } else if (!is.null(d)) {
        paste0(cl, "[", paste0(d, collapse = ", "), "]")
    } else if (n != 1 || (is.list(x) && !is.object(x))) {
        paste0(cl, "(", n, ")")
    } else if (is.character(x) && !is.object(x)) {
        wellipsis <- 1
        chars <- if (is.na(width)) .Machine$integer.max else (width - wellipsis)
        utf8_format(x, chars = chars)
    } else if (is.function(x) || is.language(x)) {
        paste0("<", cl, ">")
    } else {
        format(x, line = width)
    }
}


format.record <- function(x, limit = NA, line = NA, meta = FALSE, ...)
{
    x     <- as.record(x)
    limit <- if (is.null(limit)) option.frame.limit() else as.size.scalar(limit)
    line  <- if (is.null(line))  option.frame.line()  else as.size.scalar(line)
    meta  <- as.option(meta)

    lfmt   <- format_record_limit(x, limit)
    trunc  <- lfmt$trunc
    nfmt   <- format_record_names(lfmt$object)
    nwidth <- nfmt$width

    width <- if (is.na(line)) NA else max(1, line - nwidth - 3)
    y <- format_record_values(nfmt$object, width)

    if (meta) {
        attr(y, "format.meta") <- list(name.width = nwidth, trunc = trunc)
    }

    y
}


format_record_lines <- function(x, name.width, indent = 2)
{
    names <- names(x)
    tot <- record_total(x)
    lines <- character(tot)
    dst <- 1

    for (i in seq_along(x)) {
        xi <- x[[i]]
        if (is.record(xi)) {
            lines[[dst]] <- paste0(utf8_encode(names[[i]], display = TRUE), ":")
            dst <- dst + 1
            xsubi <- format_record_lines(xi, name.width - indent, indent)
            nsub <- length(xsubi)
            prefix <- formatC("", width = indent)
            if (nsub > 0) {
                lines[dst:(dst + nsub - 1)] <- paste0(prefix, xsubi)
                dst <- dst + nsub
            }
        } else {
            prefix <- utf8_encode(names[[i]], width = name.width,
                                  display = TRUE)
            suffix <- utf8_encode(xi, display = TRUE)
            lines[[dst]] <- paste0(prefix, " : ", suffix)
            dst <- dst + 1
        }
    }

    lines
}


print.record <- function(x, limit = NULL, line = NULL, ...)
{
    x <- as.record(x)

    if (length(x) == 0) {
        cat("{}\n")
    } else {
        fmt <- format.record(x, limit, line, meta = TRUE)
        meta <- attr(fmt, "format.meta")
        lines <- format_record_lines(fmt, meta$name.width)
        if (length(lines) > 0) {
            cat(lines, sep = "\n")
        }

        if (meta$trunc) {
            total <- record_total(x)
            prefix <- utf8_encode("...", width = meta$name.width + 3)
            suffix <- paste0("(", total, " entries total)")
            cat(prefix, suffix, "\n", sep = "")
        }
    }

    invisible(x)
}
