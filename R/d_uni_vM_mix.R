#' Mixture Density Function of Uniform and von Mises
#'
#' Density function for the mixture of the uniform and von Mises distribution.
#' @param x A numeric vector. Is coerced to class \code{\link{circular}}.
#' @param mu Mean direction of the distribution. Is coerced to class \code{\link{circular}}.
#' @param kappa Non-negative numeric value for the concentration parameter of the distribution.
#' @param p_unif A numeric between 0 and 1. The probability of observing a point from the uniform distribution.
#' @return The density at the locations given by \code{x}.
#' @import circular
#' @importFrom stats dunif
#' @export
d_uni_vM_mix <- function(x, mu, kappa, p_unif) {
  p_unif * dunif(x, min = 0, max = 2 * pi) + (1 - p_unif) * dvonmises(x = circular(x), mu = circular(mu), kappa = kappa)
}
