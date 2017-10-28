#' Create point pattern
#'
#' Creates an object of class \code{ppc} representing a point pattern dataset on the unit circle.
#' @param x Vector of polar angles or cartesian x coordinates of the data points.
#' @param y Vector of cartesian y coordinates. Should be specified only if \code{x} represent cartesian coordiante
#' @param window Window of observation, an object of class \code{\link{cirwin}}.
#' @param check Logical value indicating wheter to check that al points lie inside the specified window.
#' @return An object of class \code{ppc} describing a point pattern on the unit circle.
#' @author Andreas Dyreborg Christoffersen \email{andreas@math.aau.dk}
#' @import spatstat
#' @export
ppc <- function(x, y, window = cirwin(), check = TRUE) {
  stopifnot(verifyclass(window, "cirwin"))
  if (missing(x)) {
    angs <- numeric(0)
    n <- 0
  } else if (missing(y)) {
    stopifnot(is.numeric(x) & is.vector(x))
    angs <- x
    n <- length(x)
  } else if (!missing(y)) {
    stopifnot(is.numeric(x) & is.vector(x))
    stopifnot(is.numeric(y) & is.vector(y))
    nx <- length(x)
    ny <- length(y)
    stopifnot(nx == ny)
    x <- car2pol(cbind(x, y), unitlength = FALSE)
    angs <- x$ang
    n <- nx
  }
  if (check == TRUE) {
    inside <- inside.cirwin(angs, window)
    n_outside <- sum(!inside)
    if(n_outside > 0) warning(paste(n_outside, "point(s) were removed due to not being contained in the specified window"))
    n <- n - n_outside
    angs <- angs[inside]
  }
  out <- ppx(data = data.frame(angs = angs, r = rep.int(1, n)),
             domain = window,
             simplify = FALSE)
  ppc <- list(data = out$data, window = out$domain, n = n)
  class(ppc) <- c("ppc", class(out))
  return(ppc)
}
