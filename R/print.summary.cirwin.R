#' @author Andreas Dyreborg Christoffersen \email{andreas@math.aau.dk}
#' @import spatstat spatstat.utils

print.summary.cirwin <- function(X, ...) {
  verifyclass(X, "summary.cirwin")
  switch(X$type,
         circle = {
           cat("Window: circle =", prange(zapsmall(c(0, 2 * pi))), "\n")
           cat("Window area =", deparse(X$area), if(X$area == 1) summary(X$units)$singular else summary(X$units)$plural)
         }, arcs = {
           cat("Window: circle =", prange(zapsmall(X$intervals[1, ])))
           for(i in 2:nrow(X$intervals)) {
             cat(" +", prange(zapsmall(X$intervals[i, ])))
           }
           cat("\n", "Window area = ", deparse(X$area), " ", if(X$area == 1) summary(X$units)$singular else summary(X$units)$plural, sep = "")
         })
  return(invisible(X))
}
