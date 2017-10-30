#' Plot \code{ppc} object
#'
#' Function for plotting an object of class ppc (Only implemented for data observed on the full circle)
#' @param x An object of class \code{\link{ppc}}. Must be observed on intire circle.
#' @param ... Additional arguments passed to \code{\link{plot.circular}}.
#' @import spatstat circular
#' @export
plot.ppc <- function(x, ...) {
  stopifnot(verifyclass(x, "ppc"))

  # if (x$window$type == "arcs") {
  #   W_min <- x$window$intervals[1]
  #   W_max <- x$window$intervals[2]
  #
  #   ## Plot the window
  #   n <- 1000
  #   frame_x <- cos(seq(W_min, W_max, length = n))
  #   frame_y <- sin(seq(W_min, W_max, length = n))
  #   plot.default(frame_x, frame_y,
  #                xlim = c(-1, 1), ylim = c(-1, 1), asp = 1,
  #                axes = FALSE, type = "l",
  #                xlab = "", ylab = "")
  #   tcl <- 0.125
  #   text(0, 0, "+", cex = 1)
  #   text(1 - tcl, 0, "0", cex = 1)
  #   text(0, 1 - tcl, expression(frac(pi, 2)), cex = 1)
  #   text(-1 + tcl, 0, expression(pi), cex = 1)
  #   text(0, -1 + tcl, expression(frac(3*pi, 2)), cex = 1)
  #   at <- c(0, pi/2, pi, 3*pi/2)
  #   at_car <- pol2car(at)
  #   lab <- c("0", expression(frac(pi, 2)), expression(pi), expression(frac(3 * pi, 2)))
  #   for (i in seq_along(at)) {
  #
  #     text(0, 1 - 0.125, lab, cex = 1)
  #   }
  # }

  if (x$window$type == "circle") {
    if (npoints(x) != 0) {
      plot.circular(circular(x$data), ...)
    } else {
      plot.circular(circular(0), pch = NA, ...)
    }
  }
}
