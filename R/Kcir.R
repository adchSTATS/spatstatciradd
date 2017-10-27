#' K-function of a three-dimensional point pattern
#'
#' Estimates the K-function from a point pattern on the circle
#' @param X Circular point pattern (object of class "sph").
#' @param ... Ignored.
#' @param r Optional.
#' Vector of values for the argument r at which \code{K(r)} should be evaluated.
#' Users are advised not to specify this argument; there is a sensible default.
#' If necessary, specify rmax.
#' @param rmax Optional. Maximum desired value of the argument r.
#' @param nrval Optional.
#' Number of values of r for which \code{K3(r)} will be estimated.
#' A large value of nrval is required to avoid discretisation effects.
#' @details No edge correction have been implemented.
#' And it is assumede that the data are observed on the entire circle, i.e. the window is the entire circle.
#' @return A function value table (object of class "fv") that can be plotted, printed or coerced to a data frame containing the function values.
#' @author Andreas Dyreborg Christoffersen \email{andreas@math.aau.dk}
#' @importFrom stats dist
#' @export
Kcir <- function(X, ..., r = NULL, rmax = NULL, nrval = 128) {
  stopifnot(inherits(X, "sph"))
  stopifnot(X$dim == "circle")
  if(X$window$type != "circle") stop("This function is only iimplemented for data observed on the whole circle")

  if(is.null(rmax) && is.null(r)) {
    rmax <- pi
    r <- seq(from = 0, to = rmax, length.out = nrval)
  }

  out <- fv(x = data.frame(r = r, theo = 2 * r),
            argu = "r",
            ylab = substitute(Kcir(r), NULL),
            valu = "theo",
            fmla = . ~ r,
            alim = c(0, max(r)),
            labl = c("r", "%s[theo](r)"),
            desc = c("distance argument r", "Theoretical %s for homogenious Poisson"),
            fname = "Kcir")

  np <- npoints(X)
  win_vol <- 2 * pi
  rho_sq_hat <- np * (np - 1) / win_vol

  dists <- as.matrix(dist(X$ang))
  dists_new <- ifelse(dists > pi, 2 * pi - dists, dists)
  K <- as.data.frame(sapply(r, function(x) sum(dists_new <= x) - np) / rho_sq_hat)
  names(K) <- "Kcir"

  out <- bind.fv(x = out,
                 y = as.data.frame(K),
                 labl = "hat(%s)[est](r)",
                 desc = "Estimate of %s",
                 preferred = "Kcir")
  unitname(out) <- unitname(X)
  return(out)
}
