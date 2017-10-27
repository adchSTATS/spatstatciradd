#' Summary of a Circular Window
#'
#' Prints a useful description of a window object of class \code{\link{cirwin}}.
#' @param object Window (object of class "cirwin").
#' @param ... Ignored.
#' @details A useful description of the window object is printed.
#' This is a method for the generic function \code{\link{summary}}.
#' @author Andreas Dyreborg Christoffersen \email{andreas@math.aau.dk}
#' @import spatstat
#' @export
summary.cirwin <- function(object, ...) {
  verifyclass(object, "cirwin")
  out <- list(type = object$type,
              intervals = object$intervals,
              area = area(object),
              units = unitname(object))
  class(out) <- "summary.cirwin"
  return(out)
}
