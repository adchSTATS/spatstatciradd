#' K-function of a three-dimensional point pattern
#'
#' Estimates the K-function from a point pattern on the circle
#' @param X Circular point pattern (object of class \code{\link{ppc}}).
#' @param r Optional.
#' Vector of values for the argument r at which \code{K(r)} should be evaluated.
#' Users are advised not to specify this argument; there is a sensible default.
#' If necessary, specify rmax.
#' @param rmax Optional. Maximum desired value of the argument r.
#' @param nrval Optional.
#' Number of values of r for which \code{K3(r)} will be estimated.
#' A large value of nrval is required to avoid discretisation effects.
#' @param intenss Optional.
#' Required for computing the inhomogeneous K function.
#' Either \code{NULL}, a numeric vector or a numerix matrix.
#' If \code{NULL} the homogeneous K function is computed.
#' If a numeric vector then it should contain estimated intensities at the observed locations.
#' If a matrix then it should contain the product of every pair of estimated intensities.
#' @param ... Ignored.
#' @details No edge correction have been implemented.
#' And it is assumede that the data are observed on the entire circle, i.e. the window is the entire circle.
#' @return A function value table (object of class \code{\link{fv}}) that can be plotted, printed or coerced to a data frame containing the function values.
#' @author Andreas Dyreborg Christoffersen \email{andreas@math.aau.dk}
#' @importFrom stats dist
#' @export
Kcir <- function(X, r = NULL, rmax = NULL, nrval = 128, intenss = NULL, ...) {
  stopifnot(inherits(X, "ppc"))
  if(X$window$type != "circle") stop("This function is only implemented for data observed on the whole circle")

  if(is.null(r)) {
    if (is.null(rmax)) {
      rmax <- pi / 2
    }
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

  dists <- as.matrix(dist(X$data$angs))
  dists_new <- ifelse(dists > pi, 2 * pi - dists, dists)

  if (is.null(intenss)) {
    recip_intenss_mat <- matrix(rep((win_vol^2) / (np * (np - 1)), np^2), ncol = np)
  } else if (is.vector(intenss)) {
    stopifnot(is.numeric(intenss))
    recip_intenss_mat <- 1 / tcrossprod(intenss)
  } else if (is.matrix(intenss)) {
    stopifnot(is.numeric(intenss))
    stopifnot(all(c(np, np) == dim(intenss)))
    recip_intenss_mat <- 1 / intenss
  }
  diag(recip_intenss_mat) <- 0

  K <- as.data.frame(sapply(r, function(d) {
    satisfied <- dists_new <= d
    diag(satisfied) <- FALSE
    sum(recip_intenss_mat[satisfied])
  }) / win_vol)
  names(K) <- "Kcir"

  out <- bind.fv(x = out,
                 y = as.data.frame(K),
                 labl = "hat(%s)[est](r)",
                 desc = "Estimate of %s",
                 preferred = "Kcir")
  unitname(out) <- unitname(X)
  return(out)
}
