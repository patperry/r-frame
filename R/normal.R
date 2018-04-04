
as.normal <- function(x)
{
    if (is.record(x)) {
        for (i in seq_along(x)) {
            x[[i]] <- as.normal(x[[i]])
        }
    } else {
        mode <- storage.mode(x)
        if (mode == "character") {
            x <- as_utf8(x)
        } else if (mode == "double") {
            x[x == 0] <- 0      # replace -0 with 0
            x[is.nan(x)] <- NaN # replace all NaN with canonical
        } else if (mode == "complex") {
            x[is.na(x)] <- NA
            re <- as.normal(Re(x))
            im <- as.normal(Im(x))
            x <- complex(real = re, imaginary = im)
        } else {
            # pass
        }
    }

    x
}
