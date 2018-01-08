
#'@method valid url.class
valid.url.class <- function(x, class2 = "url.class") {
    if (typeof(x) != "character") 
        return(F)
    if (length(x) != 5) 
        return(F)
    return(T)
}

#'url.class
#'@export
url.class <- function(x, ...) {
    UseMethod("url.class", x)
}

RFC3986Regex <- "^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?"
urlElems <- c("\\2", "\\4", "\\5", "\\7", "\\9")

#'@method url.class default
url.class.default <- function(x, ...) {
    if (length(x) == 1) {
        x <- sapply(urlElems, gsub, pattern = RFC3986Regex, x = x)
    }
    if (valid.url.class(x) != TRUE) 
        stop()
    class(x) <- "url.class"
    names(x) <- c("service", "authority", "path", "query", "fragment")
    x
}
