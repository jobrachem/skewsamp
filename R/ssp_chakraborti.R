#' Estimate N on the basis of two pilot samples.
#'
#' Chakraborti, Hong, & van de Wiel (2006)
#'
#' Chakraborti, S., Hong, B., & van de Wiel, M. A. (2006). A note on
#' sample size determination for a nonparametric test of location.
#' Technometrics, 48(1), 88–94. https://doi.org/10.1198/004017005000000193
#'
#' @param s1,s2 Pilot samples
#' @param delta numeric value, location shift parameter \eqn{\delta}
#' @param alpha Type I error probability
#' @param power 1 - Type II error probability, the desired statistical
#'   power
#'
#' @return Returns an object of class \code{"sample_size"}. It contains
#'   the following components:
#'   \item{n}{the total sample size}
#'   \item{n1}{sample size in Group 1 (control group)}
#'   \item{n2}{sample size in Group 2 (treatment group)}
#'   \item{two_sided}{logical, \code{TRUE}, if the estimated sample size
#'     refers to a two-sided test}
#'   \item{alpha}{type I error rate used in sample size estimation}
#'   \item{power}{target power used in sample size estimation}
#'   \item{effect}{effect size used in sample size estimation}
#'   \item{effect_type}{short description of the type of effect size}
#'   \item{comment}{additional comment, if there is any}
#'   \item{call}{the matched call.}
#'
#' @examples
#' n_locshift(s1 = rexp(10), s2 = rexp(10),
#'            alpha = 0.05, power = 0.9, delta = 0.35)
#' @export
n_locshift <- function(s1, s2, delta, alpha, power) {
  n1 <- n_locshift_one(s1, alpha, power, delta)
  n2 <- n_locshift_one(s2, alpha, power, delta)

  n <- (n1 + n2) / 2

  comment <- "Wilcoxon-Mann-Whitney Test, Location shift"
  n <- sample_size(n * 2, two_sided = FALSE, alpha = alpha, power = power,
                   effect = delta, effect_type = "location shift",
                   q = 0.5,
                   comment = comment,
                   call = match.call()
  )
  n
}


#' Compute a distribution of estimates of N based on two pilot samples.
#'
#' Chakraborti, Hong, & van de Wiel (2006)
#'
#' Chakraborti, S., Hong, B., & van de Wiel, M. A. (2006). A note on
#' sample size determination for a nonparametric test of location.
#' Technometrics, 48(1), 88–94. https://doi.org/10.1198/004017005000000193
#'
#' @param s1,s2 Pilot samples
#' @param n_resamples number of resamples to use in bootstrapping
#' @param delta numeric value, location shift parameter \eqn{\delta}
#' @param alpha Type I error probability
#' @param power 1 - Type II error probability, the desired statistical
#'   power
#' @export
#'
#' @return numeric vector of sample size estimates
resample_n_locshift <- function(s1, s2, delta, alpha, power, n_resamples = 500) {
  resamples_s1 <- resample_n_locshift_one(s1, alpha, power, delta, n_resamples)
  resamples_s2 <- resample_n_locshift_one(s2, alpha, power, delta, n_resamples)

  resamples <- ceiling((resamples_s1 + resamples_s2) / 2)
  resamples
}

#' Compute an upper bound the sample size based on two pilot samples.
#'
#' Chakraborti, Hong, & van de Wiel (2006)
#'
#' Chakraborti, S., Hong, B., & van de Wiel, M. A. (2006). A note on
#' sample size determination for a nonparametric test of location.
#' Technometrics, 48(1), 88–94. https://doi.org/10.1198/004017005000000193
#'
#' @param s1,s2 Pilot samples
#' @param delta numeric value, location shift parameter \eqn{\delta}
#' @param alpha Type I error probability
#' @param power 1 - Type II error probability, the desired statistical
#'   power
#' @param n_resamples number of resamples to use in bootstrapping
#' @param q Quantile to use as the upper bound.
#' @export
#'
#' @return Returns an object of class \code{"sample_size"}. It contains
#'   the following components:
#'   \item{n}{the total sample size}
#'   \item{n1}{sample size in Group 1 (control group)}
#'   \item{n2}{sample size in Group 2 (treatment group)}
#'   \item{two_sided}{logical, \code{TRUE}, if the estimated sample size
#'     refers to a two-sided test}
#'   \item{alpha}{type I error rate used in sample size estimation}
#'   \item{power}{target power used in sample size estimation}
#'   \item{effect}{effect size used in sample size estimation}
#'   \item{effect_type}{short description of the type of effect size}
#'   \item{comment}{additional comment, if there is any}
#'   \item{call}{the matched call.}
#'
#' @examples
#' n_locshift_upper_bound(s1 = rexp(10), s2 = rexp(10),
#'            delta = 0.35, alpha = 0.05, power = 0.9)
n_locshift_upper_bound <- function(s1, s2, delta, alpha, power, q = 0.9, n_resamples = 500) {
  resamples <- resample_n_locshift(s1, s2, delta, alpha, power, n_resamples)

  n <- stats::quantile(resamples, type = 3, probs = q)

  note1 <- "Wilcoxon-Mann-Whitney Test, Location shift\n"
  note2 <- "NOTE: This is an intentionally conservative UPPER BOUND estimate."
  comment <- paste0(note1, note2)

  n <- sample_size(n, two_sided = FALSE,
                   alpha = alpha, power = power,
                   effect = delta, effect_type = "location shift",
                   comment = comment, call = match.call())

  n
}
