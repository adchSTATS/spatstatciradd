#' Cartesian to polar coordinates
#' 
#' Function for transforming cartesian coordinates to polar coordinates.
#' @param X A numerical vector or matrix whos entries constitute a cartesian coordinates.
#' A vector should have 2 entries and a matrix should have 2 columns.
#' @param deg Logical value indicating whether the output should be in randians or degrees. Radian is default.
#' @param unitlength Logical values indicating if the points should be returned with unit length. 
#' If TRUE (default) \code{r} will be set to 1.
#' @return A data.frame with 3 columns longitude, latitude and radial distance.
#' @author Andreas Dyreborg Christoffersen \email{andreas@math.aau.dk}
#' @export
car2pol <- function(X, deg = FALSE, unitlength = TRUE) {
  stopifnot(length(X) == 2 || ncol(X) == 2)
  if(!is.null(dim(X))){
    x <- X[, 1]
    y <- X[, 2]
  } else {
    x <- X[1]
    y <- X[2]
  }
  r <- if(unitlength) 1 else sqrt(x^2 + y^2)
  ang <- atan2(y, x) %% (2 * pi)
  if(deg) {
    ang <- ang * 180 / pi
  }
  out <- data.frame(ang = ang, r = r)
  return(out)
}
