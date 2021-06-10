#' Empirical cumulative density function (ECDF)
#'
#' @param q numeric vector of values to evaluate
#' @param sample numeric vector of sample values to base the ECDF on
#'
#'
#' @return Returns the probabilities that a value drawn at random from the
#'   empirical cumulative density based on *sample* is smaller than or
#'   equal to the elements of x.
#' @export
#'
#' @examples
#' x <- 1:5
#' pemp(1, x)
pemp <- function(q, sample) {
  m <- length(sample)
  if (m < 2) stop("sample must have length >= 2")
  xsample <- extend_sample(sample)

  i <- find_smaller_index(q, xsample) - 1

  lower_neighbours <- xsample[i + 1]
  upper_neighbours <- xsample[i + 2]

  term1 <- i / (m + 1)
  term2a <- q - lower_neighbours
  term2b <- (m + 1) * (upper_neighbours - lower_neighbours)

  p <- term1 + (term2a / term2b)
  p <- ifelse(q <= xsample[1], 0, ifelse(q >= xsample[m + 2], 1, p))

  p
}


#' Empirical probability density function (EPDF)
#'
#' @param x numeric vector of values to evaluate
#' @param sample numeric vector of sample values to base the EPDF on
#'
#' @return numeric vector of density values based on the EPDF
#' @export
#'
#' @examples
#' x <- 1:5
#' demp(1, x)
demp <- function(x, sample) {
  m <- length(sample)
  if (m < 2) stop("sample must have length >= 2")

  xsample <- extend_sample(sample)

  lower_ext <- xsample[1]
  upper_ext <- xsample[m + 2]

  i <- find_smaller_index(x, xsample) - 1 # start at 0
  lower_neighbours <- xsample[i + 1]
  upper_neighbours <- xsample[i + 2]

  density_value <- 1 / ((m + 1) * (upper_neighbours - lower_neighbours))

  exceeds_boundary <- x < lower_ext | x >= upper_ext
  density_value <- ifelse(exceeds_boundary, 0, density_value)
  density_value
}


#' Empirical quantile function
#'
#' @param p probability
#' @param sample numeric vector of sample values to base the ECDF on
#'
#' @return Returns the value for which \code{pemp(x, sample) = p},
#'   i.e. the probability that a value drawn at random from the ECDF
#'   is smaller or equal to \code{x} is \code{p}.
#' @export
#'
#' @examples
#' x <- 1:5
#' qemp(0.1, x)
qemp <- function(p, sample) {

  if (any(p < 0 | p > 1)) stop("p must be between 0 and 1")

  m <- length(sample)
  xsample <- extend_sample(sample)

  empirical_p <- pemp(xsample, sample)
  i <- find_smaller_index(p, empirical_p) - 1

  lower_neighbours <- xsample[i + 1]
  upper_neighbours <- xsample[i + 2]

  term1 <- (p * (m + 1)) - i
  term2 <- upper_neighbours - lower_neighbours

  x <- (term1 * term2) + lower_neighbours
  x <- ifelse(p == 0, xsample[1], ifelse(p == 1, xsample[m + 2], x))
  x
}


#' Draws random values from the ECDF obtained from \code{sample}
#'
#' @param n integer, number of samples to be drawn
#' @param sample numeric vector of sample values to base the ECDF on
#'
#' @return numeric vector of random values drawn from the ECDF
#' @export
#'
#' @examples
#' x <- 1:5
#' remp(10, x)
remp <- function(n, sample) {
  p <- stats::runif(n)
  qemp(p, sample)
}
