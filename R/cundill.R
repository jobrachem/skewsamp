#' Calculate sample size for a group comparison via generalized linear models
#'
#' Estimation of required sample size as given by Cundill & Alexander (2015).
#'
#' @param mean0 Mean in control group
#' @param mean1 Mean in treatment group
#' @param dispersion0 Dispersion parameter in control group
#' @param dispersion1 Dispersion parameter in treatment group.
#' @param alpha Type I error rate
#' @param power 1 - Type II error rate
#' @param link_fun function object, the link function to create the
#'   response \eqn{\eta}.
#' @param variance_fun function object, function for computing the
#'   variance based on a mean and a dispersion parameter
#' @param dmu_deta_fun function object, derivative of the original
#'   mean with respect to the link: \eqn{d\mu / d\eta}.
#' @param q Number between 0 and 1, the proportion of observations
#'   allocated to the control group
#'
#' @return Total sample size (numeric)
#' @references
#' Cundill, B., & Alexander, N. D. E. (2015). Sample size calculations
#' for skewed distributions. \emph{BMC Medical Research Methodology},
#' 15(1), 1–9. https://doi.org/10.1186/s12874-015-0023-0
#' @export
#'
n_glm <- function(
  mean0, mean1,
  dispersion0, dispersion1,
  alpha, power,
  link_fun = function(mu) NULL,
  variance_fun = function(mu, dispersion) NULL,
  dmu_deta_fun = function(mu) NULL,
  q) {

  stopifnot(alpha >= 0, alpha <= 1)
  stopifnot(power >= 0, power <= 1)

  q0 <- q
  q1 <- 1 - q0

  frac <- function(mu, dispersion, q) variance_fun(mu, dispersion) / ((dmu_deta_fun(mu)^2) * q)

  normal_quantiles <- stats::qnorm(1 - alpha / 2) + stats::qnorm(power)
  variance_term <- frac(mean1, dispersion1, q1) + frac(mean0, dispersion0, q0)
  response_diff <- link_fun(mean0) - link_fun(mean1)

  n_root <- (normal_quantiles * sqrt(variance_term)) / response_diff
  n_root^2
}


#' Calculate sample size for gamma distribution
#'
#' Estimation of required sample size as given by Cundill & Alexander (2015).
#'
#' @param mean0 Mean in control group
#' @param effect Effect size, \eqn{1 - (\mu_1 / \mu_0)}, where
#'   \eqn{\mu_0} is the mean in the control group (\code{mean0}) and
#'   \eqn{\mu_1} is the mean in the treatment group.
#' @param shape0 Shape parameter in control group
#' @param shape1 Shape parameter in treatment group. Defaults to
#'   \code{shape0}, because GLM assumes equal shape across groups.
#' @param alpha Type I error rate
#' @param power 1 - Type II error rate
#' @param q Proportion of observations allocated to the control group
#' @param link Link function to use. Currently implement: 'log' and 'identity'
#' @param two_sided logical, if \code{TRUE} the sample size
#'   will be calculated for a two-sided test. Otherwise, the sample
#'   size will be calculated for a one-sided test.
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
#' Cundill, B., & Alexander, N. D. E. (2015). Sample size calculations
#' for skewed distributions. \emph{BMC Medical Research Methodology},
#' 15(1), 1–9. https://doi.org/10.1186/s12874-015-0023-0
#'
#' @examples
#' n_gamma(mean0 = 8.46, effect = 0.7, shape0 = 0.639,
#'            alpha = 0.05, power = 0.9)
#' @export
#'
n_gamma <- function(mean0, effect, shape0, shape1 = shape0,
                    alpha = 0.05, power = 0.9, q = 0.5,
                    link = c("log", "identity"),
                    two_sided = TRUE
) {

  link <- match.arg(link)
  dmu_deta_fun <- dmu_deta_funs[[link]]
  link_fun <- linkfuns[[link]]

  alpha <- ifelse(two_sided, alpha, alpha * 2)

  mean1 <- mean0 * (1 - effect)
  dispersion0 <- shape0
  dispersion1 <- shape1

  variance_fun <- function(mu, dispersion) mu^2 / dispersion

  n <- n_glm(mean0, mean1, dispersion0, dispersion1,
             alpha, power,
             link_fun, variance_fun, dmu_deta_fun = dmu_deta_fun,
             q)

  comment <- paste("Generalized Regression, Gamma Distribution, link:", link)
  n <- sample_size(n, two_sided = TRUE, alpha = alpha, power = power,
                   effect = effect, effect_type = "1 - (mean1/mean0)",
                   q = q,
                   comment = comment,
                   call = match.call()
  )

  n
}

#' Calculate sample size for negative binomial distribution
#'
#' Estimation of required sample size as given by Cundill & Alexander (2015).
#'
#' @param dispersion0 Dispersion parameter in control group
#' @param dispersion1 Dispersion parameter in treatment group. Defaults to
#'   \code{shape0}, because GLM assumes equal shape across groups.
#' @inheritParams n_gamma
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
#' Cundill, B., & Alexander, N. D. E. (2015). Sample size calculations
#' for skewed distributions. \emph{BMC Medical Research Methodology},
#' 15(1), 1–9. https://doi.org/10.1186/s12874-015-0023-0
#'
#' @examples
#' n_negbinom(mean0 = 71.4, effect = 0.7, dispersion0 = 0.33,
#'            alpha = 0.05, power = 0.9)
#' @export
n_negbinom <- function(mean0, effect,
                       dispersion0, dispersion1 = dispersion0,
                       alpha = 0.05, power = 0.9, q = 0.5,
                       link = c("log", "identity"),
                       two_sided = TRUE) {

  link <- match.arg(link)
  dmu_deta_fun <- dmu_deta_funs[[link]]
  link_fun <- linkfuns[[link]]

  alpha <- ifelse(two_sided, alpha, alpha * 2)

  mean1 <- mean0 * (1 - effect)
  variance_fun <- function(mu, dispersion) mu + (mu^2 / dispersion)

  n <- n_glm(mean0, mean1, dispersion0, dispersion1,
             alpha, power,
             link_fun, variance_fun, dmu_deta_fun,
             q)

  comment <- paste("Generalized Regression, Negative Binomial Distribution, link:", link)
  n <- sample_size(n, two_sided = TRUE, alpha = alpha, power = power,
                   effect = effect, effect_type = "1 - (mean1/mean0)",
                   q = q,
                   comment = comment,
                   call = match.call()
  )
  n
}


#' Calculate sample size for poisson distribution
#'
#' Estimation of required sample size as given by Cundill & Alexander (2015).
#'
#' @inheritParams n_gamma
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
#' Cundill, B., & Alexander, N. D. E. (2015). Sample size calculations
#' for skewed distributions. \emph{BMC Medical Research Methodology},
#' 15(1), 1–9. https://doi.org/10.1186/s12874-015-0023-0
#'
#' @examples
#' n_poisson(mean0 = 5, effect = 0.3)
#' @export
n_poisson <- function(mean0, effect,
                      alpha = 0.05, power = 0.9, q = 0.5,
                      link = c("log", "identity"),
                      two_sided = TRUE) {

  link <- match.arg(link)
  dmu_deta_fun <- dmu_deta_funs[[link]]
  link_fun <- linkfuns[[link]]

  alpha <- ifelse(two_sided, alpha, alpha * 2)

  mean1 <- mean0 * (1 - effect)
  variance_fun <- function(mu, dispersion) mu

  dispersion0 <- dispersion1 <- NULL

  n <- n_glm(mean0, mean1, dispersion0, dispersion1,
             alpha, power,
             link_fun, variance_fun, dmu_deta_fun,
             q)

  comment <- paste("Generalized Regression, Poisson Distribution, link:", link)
  n <- sample_size(n, two_sided = TRUE, alpha = alpha, power = power,
                   effect = effect, effect_type = "1 - (mean1/mean0)",
                   q = q,
                   comment = comment,
                   call = match.call()
  )
  n
}


#' Calculate sample size for binomial distribution
#'
#' Estimation of required sample size as given by Cundill & Alexander (2015).
#'
#' @param p0 probability of success in group0
#' @param size number of trials (greater than zero)
#' @inheritParams n_gamma
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
#' Cundill, B., & Alexander, N. D. E. (2015). Sample size calculations
#' for skewed distributions. \emph{BMC Medical Research Methodology},
#' 15(1), 1–9. https://doi.org/10.1186/s12874-015-0023-0
#'
#' @examples
#' n_binom(p0 = 0.5, effect = 0.3)
#' @export
n_binom <- function(p0,
                    effect,
                    size = 1,
                    alpha = 0.05, power = 0.9, q = 0.5,
                    link = c("logit", "identity"),
                    two_sided = TRUE) {

  if (!(p0 >= 0 & p0 <= 1)) stop("p0 must lie between 0 and 1.")
  if (!(size > 0)) stop("size must be > 0")

  link <- match.arg(link)
  dmu_deta_fun <- dmu_deta_funs[[link]]
  link_fun <- linkfuns[[link]]

  alpha <- ifelse(two_sided, alpha, alpha * 2)

  odds1 <- p0 / (1 - p0) / (1 - effect)
  mean1 <- odds1 / (1 + odds1)
  mean0 <- p0

  variance_fun <- function(mu, dispersion) (mu * (1 - mu)) / dispersion

  dispersion0 <- dispersion1 <- size

  n <- n_glm(mean0, mean1, dispersion0, dispersion1,
             alpha, power,
             link_fun, variance_fun, dmu_deta_fun,
             q)

  comment <- paste("Generalized Regression, Binomial Distribution, link:", link)
  n <- sample_size(n, two_sided = TRUE, alpha = alpha, power = power,
                   effect = effect, effect_type = "1 - Odds Ratio",
                   q = q,
                   comment = comment,
                   call = match.call()
  )
  n
}

linkfuns <- list(
  "log" = log,
  "logit" = function(x) log(x / (1 - x)),
  "identity" = identity
)

dmu_deta_funs <- list(
  "log" = function(mu) mu,
  "identity" = function(mu) 1,
  "logit" = function(mu) mu * (1 - mu)
)
