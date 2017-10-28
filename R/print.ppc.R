#' Print Brief Details of a Point Pattern Dataset
#'
#' Prints a very brief description of a point pattern dataset of class \code{\link{ppc}}.
#' @param x Point pattern on the unit circle (object of class \code{\link{ppc}})
#' @param ... Ignored
#' @return A very brief description of the point pattern \code{x} is printed.
#' @author Andreas Dyreborg Christoffersen \email{andreas@math.aau.dk}
#' @export
print.ppc <- function(x, ...) {
  verifyclass(x, "ppc")
  n <- x$n
  cat("Point pattern on the", x$dim, ": ", n, " points.", fill = TRUE)
  print(x$window)
}
