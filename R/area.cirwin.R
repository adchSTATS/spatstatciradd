#' Area of a Window
#'
#' Computes the area of a window
#' @param W A window, whose area will be computed.
#' This should be an object of class \code{\link{cirwin}}.
#' @return A numerical value giving the area of the window
#' @author Andreas Dyreborg Christoffersen \email{andreas@math.aau.dk}
#' @import spatstat
#' @export
area.cirwin <- function(W) {
  verifyclass(W, "cirwin")
  switch(W$type,
         circle = {
           area <- 2 * pi
         }, arcs = {
           area <- sum(abs(W$intervals[, 2] - W$intervals[, 1]))
         }, stop("Unrecognised window type"))
  return(area)
}
