#' The general formula for sample size calculation as published by
#' Cundill & Alexander (2015)
#'
#' @param spec A list that holds the necessary specification.
#'
#' @return Numeric value, the required sample size
#' @export
#'
n_general <- function(spec = list(mean0 = NULL,
                                  mean1 = NULL,
                                  q0 = NULL,
                                  q1 = NULL,
                                  dmu_eta = NULL,
                                  variance = NULL,
                                  linkfun = NULL)
                      ) {
  frac <- function(mu) spec$variance(mu, spec) / (spec$dmu_deta(mu))^2

  normal_quantiles <- stats::qnorm(1 - spec$alpha / 2) + stats::qnorm(spec$power)
  variance_term <- frac(spec$mean1) / spec$q1 + frac(spec$mean0) / spec$q0
  response_diff <- spec$linkfun(spec$mean0) - spec$linkfun(spec$mean1)

  n_root <- (normal_quantiles * sqrt(variance_term)) / response_diff
  n_root^2
}


#' Calculate sample size for gamma distribution
#'
#' @param mean0 Mean in control group
#' @param shape Shape, assumed equal across groups
#' @param efficacy Effect size (1 - mean0/mean1)
#' @param alpha Type I error rate
#' @param power 1 - Type II error rate
#' @param q Proportion of observations allocated to the control group
#' @param link Link function
#'
#' @return Required number of observations
#' @export
#'
n_gamma <- function(mean0, shape, efficacy, alpha = 0.05, power = 0.9, q = 0.5, link = log) {

  spec <- list(mean0 = mean0, shape = shape, alpha = alpha, power = power)

  spec$mean1 <- mean0 / (1 - efficacy)
  spec$q0 <- q
  spec$q1 <- 1 - q
  spec$variance <- function(mu, spec) mu^2 / spec$shape
  spec$dmu_deta <- function(mu) mu
  spec$linkfun <- link

  n <- n_general(spec)

  n
}

