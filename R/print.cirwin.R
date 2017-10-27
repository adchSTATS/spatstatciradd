#' Print Brief Details of a Spatial Window
#'
#' Prints a very brief description of a window object of class \code{\link{cirwin}}.
#' @param x Window (object of class "\code{cirwin}")
#' @param ... Ignored
#' @return A very brief description of the window \code{x} is printed.
#' @author Andreas Dyreborg Christoffersen \email{andreas@math.aau.dk}
#' @import spatstat
#' @export
print.cirwin <- function(x, ...) {
  verifyclass(x, "cirwin")
  switch(x$type,
         circle = {
           cat("Window: complete circle = [0, 2*pi] radian")
         },
         arcs = {
           int <- round(x$intervals, digits = 2)
           cat("Window: fragmented circle = ")
           cat("[", int[1, 1], ",", int[1, 1], "]", sep = "")
           for(i in 2:nrow(int)) {
             cat(" x ", "[", int[i, 1], ",", int[i, 1], "]", sep = "")
           }
         })
}
