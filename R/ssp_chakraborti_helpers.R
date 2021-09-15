#' Noether's (1987) formula for obtaining a sample size estimation for
#' the two-sample Wilcoxon Mann-Whitney test.
#'
#' @param alpha Type I error probability
#' @param power 1 - Type II error probability, the desired statistical
#'   power
#' @param p probability \eqn{P(X < Y)} that a random observation from
#'   group X is smaller than a random observation from group Y
#' @param q relative sample size of the X-sample. Number between 0 and 1.
#'   q = 0.5 means that the size of the X-sample will be 50% of the
#'   total sample size
#'
#' @return estimated required total sample size
#' @keywords internal
n_noether <- function(alpha, power, p, q = 0.5) {
  term1 <- (stats::qnorm(1 - alpha) + stats::qnorm(power))^2
  term2 <- 12 * q * (1 - q) * (p - 0.5)^2

  term1 / term2
}


#' Computes an empirical estimate of \code{p} (\eqn{P(X < X + \delta)})
#'
#' @param sample numeric vector of data to base the estimation on (X)
#' @param delta numeric value, location shift parameter \eqn{\delta}
#'
#' @return An empirical estimate of \eqn{P(X < X + \delta)}
#' @keywords internal
estimate_p <- function(sample, delta) {
  m <- length(sample)

  xsample <- extend_sample(sample)
  shifted_sample <- xsample[-1] - delta
  combined_sample <- sort(c(xsample, shifted_sample))

  p <- 0
  for (i in seq(2*m + 2)) {
    lwr <- combined_sample[i]
    upr <- combined_sample[i + 1]

    term1 <- demp(lwr, sample) * (upr - lwr)
    term2 <- (pemp(upr + delta, sample) + pemp(lwr + delta, sample)) / 2

    p <- p + (term1 * term2)
  }

  p
}


#' Estimate N on the basis of one pilot sample.
#'
#' @param sample pilot data
#' @param alpha Type I error probability
#' @param power 1 - Type II error probability, the desired statistical
#'   power
#' @param delta numeric value, location shift parameter \eqn{\delta}
#' @param q size of group0 relative to total sample size.
#'
#' @return numeric value, an estimate of the sample size required to
#'   detect a location shift of \code{delta} with a Wilcoxon Mann-Whitney
#'   test with \code{power} and \code{alpha}.
#' @keywords internal
n_locshift_one <- function(sample, alpha, power, delta, q) {
  if (!(q > 0 & q < 1)) stop("q must be between 0 and 1.")

  p <- estimate_p(sample, delta)
  n <- n_noether(alpha, power, p, q)
  n
}


#' Compute \code{n_resamples} estimates of N
#'
#' @param sample pilot data
#' @param n_resamples number of resamples to use in bootstrapping
#' @param alpha Type I error probability
#' @param power 1 - Type II error probability, the desired statistical
#'   power
#' @param delta numeric value, location shift parameter \eqn{\delta}
#' @param q size of group0 relative to total sample size.
#'
#' @return numeric vector of sample size estimates
#' @keywords internal
resample_n_locshift_one <- function(sample, alpha, power, delta, n_resamples, q) {
  m <- length(sample)
  resamples <- remp(m * (n_resamples - 1), sample)
  resamples <- matrix(c(sample, resamples), ncol = m, byrow = TRUE)

  resampled_n_estimates <- apply(resamples, 1, n_locshift_one,
                                 alpha = alpha,
                                 power = power,
                                 delta = delta,
                                 q = q)
  resampled_n_estimates
}
