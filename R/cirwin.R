#' Create a window on the circle
#' 
#' Creates an object of class "\code{cirwin}" representing an observation window on the circle
#' @param type The type of window to be created. 
#' Can be either the full circle or a subset and should be "\code{circle}" respectively "\code{arcs}".
#' If type is "\code{circle}" the second argument \code{intervals} will be ignored
#' @param intervals Only required for \code{type="arcs"}. 
#' A vector or matrix like object with length 2 respectively number of columns equal to \code{2*n}, where \code{n} is some positive integer
#' If specified as a vector (or matrix) the entries (columns) should be alternating between lower and upper bound, starting with the lower bound.
#' @return An object of class \code{cirwin}, which contains information regarding the window specified
#' @author Andreas Dyreborg Christoffersen \email{andreas@math.aau.dk}
#' @export
cirwin <- function(type = "circle", intervals) {
  posstypes <- c("circle", "arcs")
  if (!(type %in% posstypes)) {
    stop(paste("Type must be one of "), paste(posstypes, 
                                              collapse = " or "))
  }
  if(!missing(intervals)) if(min(intervals) < 0 || max(intervals) > 2 * pi) {
    stop("Entries of 'intervals' are considered angles and should be contained in the interval (0, 2 * pi) accordingly ")
  }
  switch(type, 
         circle = {
           intervals <- c(0, 2 * pi)
         }, 
         arcs = {
           if(!is.null(dim(intervals))) {
             if(ncol(intervals) == 1) {
               intervals <- as.vector(intervals)
             } else if(ncol(intervals) %% 2 == 0){
               intervals <- matrix(t(intervals), ncol = 2, byrow = TRUE)
             } else stop("Number of columns of 'intervals' is not a multiple of 2")
           }
           if(is.vector(intervals)) {
             if(length(intervals) %% 2 == 0) {
               intervals <- matrix(intervals, ncol = 2, byrow = TRUE)
             } else stop("length of 'intervals' is not a multiple of 2")
           }
         })
  out <- list(type = type, intervals = intervals)
  class(out) <- "cirwin"
  return(out)
}
