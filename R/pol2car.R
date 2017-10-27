#' Polar to cartesian coordinates
#' 
#' Function for transforming polar coordinates to cartesian coordinates
#' @param X A numerical vector or matrix whos entries constitute polar coordinates.
#' If \code{X} is a matrix it should have two columns, consituting the angle and radial distance.
#' If \code{X} is a vector it is assumed that all entries are to be considered as angles.
#' @param deg Logical value indicating whether the input is in randians or degrees. Radian is default.
#' @param rep Determine the representation of the polar coordinates.
#' If \code{"std"} the angle will range from \code{0} to \code{2 * pi}.
#' The classical representation of polar coordinates in radians.
#' If \code{"neg"} the angle will range from \code{-pi} to \code{pi}.
#' Deafult is "std".
#' @return A \code{data.frame} with 2 columns representing the cartesian coordinate set.
#' @author Andreas Dyreborg Christoffersen \email{andreas@math.aau.dk}
#' @export
pol2car <- function(X, deg = FALSE, rep = "std") {
  stopifnot(is.vector(X) || ncol(X) == 2)
  r_specified <- !is.vector(X)
  if(!is.null(dim(X))){
    ang <- X[, 1]
    r <- if(r_specified) X[, 2] else 1
  } else {
    ang <- X
    r <- 1
  }
  if(deg){
    ang <- ang * pi / 180
  }
  if(rep == "neg") {
    ang <- ang %% (2 * pi)
  } else if(rep != "std") {
    stop(paste(sQuote("rep"), "should be", sQuote("std"), "or", sQuote("neg")))
  }
  out <- r * cbind(cos(ang), sin(ang))
  out <- as.data.frame(ifelse(abs(out) < .Machine$double.eps, 0, out))
  colnames(out) <- c("x", "y")
  return(out)
}