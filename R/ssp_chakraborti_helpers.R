#' Noether's (1987) formula for obtaining a sample size estimation for
#' the Wilcoxon Mann-Whitney test.
#'
#' @param alpha Type I error probability
#' @param power 1 - Type II error probability, the desired statistical
#'   power
#' @param p probability \eqn{P(X < Y)} that a random observation from
#'   group X is smaller than a random observation from group Y
#'
#' @return estimated required sample size
n_noether <- function(alpha, power, p) {
  term1 <- (stats::qnorm(1 - alpha) + stats::qnorm(power))^2
  term2 <- 6 * (p - 0.5)^2

  term1 / term2
}


#' Computes an empirical estimate of \code{p} (\eqn{P(X < X + \delta)})
#'
#' @param sample numeric vector of data to base the estimation on (X)
#' @param delta numeric value, location shift parameter \eqn{\delta}
#'
#' @return An empirical estimate of \eqn{P(X < X + \delta)}
estimate_one_p <- function(sample, delta) {
  m <- length(sample)
  shifted_sample <- sample + delta
  combined_sample <- extend_sample(c(sample, shifted_sample))

  h <- function(x) pemp(x, shifted_sample) * demp(x, sample)

  niter <- (2 * m) + 1
  integral_values <- vector(mode = "numeric", niter)

  for (i in seq(niter)) {
    lower <- combined_sample[i]
    upper <- combined_sample[i + 1]
    integral <- stats::integrate(h, lower, upper, stop.on.error = FALSE)

    integral_values[i] <- integral$value
  }

  p <- sum(integral_values)
  p
}


#' Estimate \code{p} (\eqn{P(X < X + \delta)}) with resampling
#'
#' Based on the given sample, this function takes \code{p_resamples}
#' resamples from the empirical CDF and estimates \eqn{P(X < X + \delta)}
#' for each of these. The return value is the arithmetic mean of these
#' estimates.
#'
#' @param sample numeric vector of data to base the estimation on (X)
#' @param delta numeric value, location shift parameter \eqn{\delta}
#' @param p_resamples number of resamples to use in the estimation of p
#'
#' @return An empirical estimate of \eqn{P(X < X + \delta)}, based on
#'   \code{p_resamples} resamples.
estimate_p <- function(sample, delta, p_resamples) {
  m <- length(sample)
  resamples <- remp(m * (p_resamples - 1), sample)
  resamples <- matrix(c(sample, resamples), ncol = m, byrow = TRUE)

  resampled_p_estimates <- apply(resamples, 1, estimate_one_p, delta = delta)

  mean(resampled_p_estimates)
}


#' Estimate N
#'
#' @param sample pilot data
#' @param alpha Type I error probability
#' @param power 1 - Type II error probability, the desired statistical
#'   power
#' @param delta numeric value, location shift parameter \eqn{\delta}
#' @param p_resamples number of resamples to use in the estimation of p
#'
#' @return numeric value, an estimate of the sample size required to
#'   detect a location shift of \code{delta} with a Wilcoxon Mann-Whitney
#'   test with \code{power} and \code{alpha}.
#' @export
#'
#' @examples
#' estimate_one_n(sample = 1:5, alpha = 0.05, power = 0.9, delta = 0.5, p_resamples = 10)
estimate_one_n <- function(sample, alpha, power, delta, p_resamples = 1) {
  p <- estimate_p(sample, delta, p_resamples)
  n <- n_noether(alpha, power, p)
  n
}


#' Compute \code{n_resamples} estimates of N
#'
#' @param sample pilot data
#' @param n_resamples number of resamples to use in bootstrapping
#' @param ... arguments passed on to \code{\link{estimate_one_n}}.
#'
#' @return numeric vector of sample size estimates
resample_one_n <- function(sample, n_resamples, ...) {
  m <- length(sample)
  resamples <- remp(m * (n_resamples - 1), sample)
  resamples <- matrix(c(sample, resamples), ncol = m, byrow = TRUE)

  resampled_n_estimates <- apply(resamples, 1, estimate_one_n, ...)
  resampled_n_estimates
}
