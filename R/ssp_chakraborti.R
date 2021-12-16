#' Estimate N on the basis of two pilot samples.
#'
#' Estimation as described by Chakraborti, Hong, & van de Wiel (2006).
#'
#' WARNING: Note that the estimation has high variability due to its dependence
#' on pilot samples. The smaller the pilot sample, the more uncertain is
#' the estimation of the required sample size. In a simulation study, we
#' found that the method may also be inaccurate on average, depending on
#' the investigated data.
#'
#' @param s1,s2 pilot samples
#' @param delta numeric value, location shift parameter \eqn{\delta}
#' @param alpha type-I error probability
#' @param power 1 - type-II error probability, the desired statistical
#'   power
#' @param q size of group0 relative to total sample size.
#'
#' @return Returns an object of class \code{"sample_size"}. It contains
#'   the following components:
#'   \item{N}{the total sample size}
#'   \item{n0}{sample size in Group 0 (control group)}
#'   \item{n1}{sample size in Group 1 (treatment group)}
#'   \item{two_sided}{logical, \code{TRUE}, if the estimated sample size
#'     refers to a two-sided test}
#'   \item{alpha}{type I error rate used in sample size estimation}
#'   \item{power}{target power used in sample size estimation}
#'   \item{effect}{effect size used in sample size estimation}
#'   \item{effect_type}{short description of the type of effect size}
#'   \item{comment}{additional comment, if there is any}
#'   \item{call}{the matched call.}
#' @references
#' Chakraborti, S., Hong, B., & van de Wiel, M. A. (2006). A note on
#' sample size determination for a nonparametric test of location.
#' Technometrics, 48(1), 88–94. https://doi.org/10.1198/004017005000000193
#'
#' @examples
#' n_locshift(s1 = rexp(10), s2 = rexp(10),
#'            alpha = 0.05, power = 0.9, delta = 0.35)
#' @export
n_locshift <- function(s1, s2, delta, alpha = 0.05, power = 0.9, q = 0.5) {
  n1 <- n_locshift_one(s1, alpha, power, delta, q)
  n2 <- n_locshift_one(s2, alpha, power, delta, q)

  m1 <- length(s1)
  m2 <- length(s2)

  n <- (n1 * m1 + n2 * m2) / (m1 + m2)

  comment <- "Wilcoxon-Mann-Whitney Test, Location shift"
  n <- sample_size(n, two_sided = FALSE, alpha = alpha, power = power,
                   effect = delta, effect_type = "location shift",
                   q = q,
                   comment = comment,
                   call = match.call()
  )
  n
}


#' Compute a distribution of estimates of N based on two pilot samples.
#'
#' Estimation of sample sizes based on resampled pilot samples from the empirical
#' cumulative density. Based on the work of Chakraborti, Hong, & van de Wiel (2006).
#'
#' WARNING: Note that the estimation has high variability due to its dependence
#' on pilot samples. The smaller the pilot sample, the more uncertain is
#' the estimation of the required sample size. In a simulation study, we
#' found that the method may also be inaccurate on average, depending on
#' the investigated data.
#'
#' @param s1,s2 Pilot samples
#' @param n_resamples number of resamples to use in bootstrapping
#' @param delta numeric value, location shift parameter \eqn{\delta}
#' @param alpha Type I error probability
#' @param power 1 - Type II error probability, the desired statistical
#'   power
#' @param q size of group0 relative to total sample size.
#' @export
#' @references
#' Chakraborti, S., Hong, B., & van de Wiel, M. A. (2006). A note on
#' sample size determination for a nonparametric test of location.
#' Technometrics, 48(1), 88–94. https://doi.org/10.1198/004017005000000193
#'
#' @return numeric vector of sample size estimates (total sample size)
resample_n_locshift <- function(s1, s2, delta, alpha = 0.05, power = 0.9, n_resamples = 500, q = 0.5) {
  resamples_s1 <- resample_n_locshift_one(s1, alpha, power, delta, n_resamples, q)
  resamples_s2 <- resample_n_locshift_one(s2, alpha, power, delta, n_resamples, q)

  m1 <- length(s1)
  m2 <- length(s2)

  resamples <- (resamples_s1 * m1 + resamples_s2 *m2) / (m1 + m2)
  resamples
}

#' Compute an upper bound the sample size based on two pilot samples.
#'
#' Based on the procedure described by Chakraborti, Hong, & van de Wiel (2006)
#'
#' WARNING: Note that the underlying estimation has high variability due
#' to its dependence on pilot samples. The smaller the pilot sample,
#' the more uncertain is
#' the estimation of the required sample size. In a simulation study, we
#' found that the underlying method may also be inaccurate on average, depending on
#' the investigated data.
#'
#' @param s1,s2 Pilot samples
#' @param delta numeric value, location shift parameter \eqn{\delta}
#' @param alpha Type I error probability
#' @param power 1 - Type II error probability, the desired statistical
#'   power
#' @param n_resamples number of resamples to use in bootstrapping
#' @param quantile Quantile to use as the upper bound.
#' @param q size of group0 relative to total sample size.
#' @export
#'
#' @return Returns an object of class \code{"sample_size"}. It contains
#'   the following components:
#'   \item{n}{the total sample size}
#'   \item{n0}{sample size in Group 0 (control group)}
#'   \item{n1}{sample size in Group 1 (treatment group)}
#'   \item{two_sided}{logical, \code{TRUE}, if the estimated sample size
#'     refers to a two-sided test}
#'   \item{alpha}{type I error rate used in sample size estimation}
#'   \item{power}{target power used in sample size estimation}
#'   \item{effect}{effect size used in sample size estimation}
#'   \item{effect_type}{short description of the type of effect size}
#'   \item{comment}{additional comment, if there is any}
#'   \item{call}{the matched call.}
#' @references
#' Chakraborti, S., Hong, B., & van de Wiel, M. A. (2006). A note on
#' sample size determination for a nonparametric test of location.
#' Technometrics, 48(1), 88–94. https://doi.org/10.1198/004017005000000193
#'
#' @examples
#' \dontrun{
#' n_locshift_bound(s1 = rexp(10), s2 = rexp(10),
#'            delta = 0.35, alpha = 0.05, power = 0.9, n_resamples = 5)}
n_locshift_bound <- function(s1, s2, delta, alpha = 0.05, power = 0.9, quantile = 0.9, n_resamples = 500, q = 0.5) {
  resamples <- resample_n_locshift(s1, s2, delta, alpha, power, n_resamples, q)

  n <- stats::quantile(resamples, type = 3, probs = quantile)

  note1 <- "Wilcoxon-Mann-Whitney Test, Location shift\n"
  note2 <- "NOTE: This is an intentionally conservative UPPER BOUND estimate."
  comment <- paste0(note1, note2)

  n <- sample_size(n, two_sided = FALSE,
                   alpha = alpha, power = power,
                   effect = delta, effect_type = "location shift",
                   q = q,
                   comment = comment, call = match.call())

  n
}
