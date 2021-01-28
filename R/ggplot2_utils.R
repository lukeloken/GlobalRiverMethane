

#' Nice log scale labels
#' 
#' modifications to scale_x_log10 to include pretty labels
#' 
#' @export
#' @name scale_log10nice
#' @rdname scale_log10nice
#' @param x character. Label for x-axis.
#' @param omag numeric. Exponents to include on log axis.
#' @import ggplot2
#' @examples 
#' library(ggplot2)
#' qplot(x=exp(5*rnorm(100)),geom="density",kernel="rectangular") + 
#'   scale_x_log10nice()
scientific_10 <- function(x) {
  xout <- gsub("1e", "10^{", format(x), fixed = TRUE)
  xout <- gsub("{-0", "{-", xout, fixed = TRUE)
  xout <- gsub("{+", "{", xout, fixed = TRUE)
  xout <- gsub("{0", "{", xout, fixed = TRUE)
  xout <- paste(xout,"}",sep="")
  return(parse(text = xout))
}

#' @export
#' @rdname scale_log10nice
#' @aliases scale_x_log10nice
#' @param omag numeric. Exponents to include on log axis.
#' @param ... additional arguments to pass to scale_x_log10
#' @import ggplot2
scale_x_log10nice <- function(omag = seq(-10,20), ...) {
  breaks10 <- 10^omag
  scale_x_log10(breaks = breaks10, labels = scientific_10(breaks10), ...)
}

#' @export
#' @rdname scale_log10nice
#' @aliases scale_y_log10nice
#' @param omag numeric. Exponents to include on log axis.
#' @param ... additional arguments to pass to scale_y_log10
#' @import ggplot2
scale_y_log10nice <- function(omag = seq(-10,20), ...) {
  breaks10 <- 10^omag
  scale_y_log10(breaks = breaks10, labels = scientific_10(breaks10), ...)
}



