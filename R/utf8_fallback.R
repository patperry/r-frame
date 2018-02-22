
utf8_fallback <- function(x, default)
{
    if (is.na(iconv(x, "UTF-8", ""))) default else x
}
