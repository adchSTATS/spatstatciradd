#' Number of points in a point pattern
#'
#' Get the number of points in a point pattern of type \code{\link{ppc}}
#' @param x A point pattern (object of class \code{\link{ppc}})
#' @return Integer
#' @author Andreas Dyreborg Christoffersen \email{andreas@math.aau.dk}
#' @export
npoints.ppc <- function(x){
  verifyclass(x, "ppc")
  x$n
}
