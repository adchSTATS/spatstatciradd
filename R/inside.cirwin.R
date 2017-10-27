#' Test whether points are inside a window on a circle
#'
#' Test whether points lie inside or outside a given window of class \code{cirwin}
#' @param points Vector of angles to be tested.
#' @param W Awindow. This should be an object of class \code{\link{cirwin}}.
#' @return A logical vector whose \code{i}th entry is \code{TRUE} if the corresponding point \code{ang[i]} is inside \code{W}.
#' @author Andreas Dyreborg Christoffersen \email{andreas@math.aau.dk}
#' @import spatstat
#' @export
inside.cirwin <- function(points, W) {
  verifyclass(W, "cirwin")
  if(length(points) == 0) return(logical(0))
  if(!is.vector(points)) stop("'points' should be a vector of angles")
  overall_OK <- points >= 0 & points <= 2*pi
  if (!any(overall_OK)) return(overall_OK)
  switch(W$type,
         circle = {
           return(overall_OK)
         },
         arcs = {
           inter <- W$intervals
           OK <- FALSE
           for(i in 1:nrow(inter)) {
             OK <- OK || inter[i, 1] <= points & inter[i, 2] >= points
           }
           OK <- overall_OK & OK
           return(OK)
         })
}
