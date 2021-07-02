#' Estimate N on the basis of two pilot samples.
#'
#' @param s1,s2 Pilot samples
#' @param delta numeric value, location shift parameter \eqn{\delta}
#' @param power 1 - Type II error probability, the desired statistical
#'   power
#' @param alpha Type I error probability
#'
#' @return numeric value, an estimate of the sample size required to
#'   detect a location shift of \code{delta} with a Wilcoxon Mann-Whitney
#'   test with \code{power} and \code{alpha}.
#' @export
#'
#' @examples
#' n_locshift(s1 = rnorm(10), s2 = rnorm(10), alpha = 0.05, power = 0.9, delta = 0.5)
n_locshift <- function(s1, s2, delta, power, alpha) {
  n1 <- n_locshift_one(s1, alpha, power, delta)
  n2 <- n_locshift_one(s2, alpha, power, delta)

  ceiling((n1 + n2) / 2)
}


#' Compute \code{n_resamples} estimates of N based on two pilot samples.
#'
#' @param s1,s2 Pilot samples
#' @param n_resamples number of resamples to use in bootstrapping
#' @param delta numeric value, location shift parameter \eqn{\delta}
#' @param power 1 - Type II error probability, the desired statistical
#'   power
#' @param alpha Type I error probability
#'
#' @return numeric vector of sample size estimates
resample_n_locshift <- function(s1, s2, n_resamples, delta, power, alpha) {
  resamples_s1 <- resample_n_locshift_one(s1, n_resamples, alpha, power, delta)
  resamples_s2 <- resample_n_locshift_one(s2, n_resamples, alpha, power, delta)

  resamples <- ceiling((resamples_s1 + resamples_s2) / 2)
  resamples
}
