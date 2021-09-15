#' Finds the index of the smaller neighbour of the given value in the vector x.
#'
#' @param value, value whose neighbour is searched
#' @param x, numeric vector
#'
#' @return integer
#' @keywords internal
find_smaller_index <- function(value, x) {
  helper <- function(value) utils::tail(which(x <= value), 1)

  as.numeric(sapply(value, helper))
}


#' Computes the lower boundary value for the empirical CDF
#'
#' @param x, a numeric vector
#'
#' @return numeric
#' @keywords internal
create_lower_extension <- function(x) {
  sorted_x <- sort(x)

  lower_ext <- 2 * sorted_x[1] - sorted_x[2]
  lower_ext
}

#' Computes the upper boundary value for the empirical CDF
#'
#' @param x, a numeric vector
#'
#' @return numeric
#' @keywords internal
create_upper_extension <- function(x) {
  m <- length(x)
  sorted_x <- sort(x)

  upper_ext <- 2 * sorted_x[m] - sorted_x[m - 1]

  upper_ext
}


#' Extends a vector by adding a lower and upper boundary.
#'
#' @param x, a numeric vector
#'
#' @return extended and sorted numeric vector
#' @keywords internal
extend_sample <- function(x) {
  lower_ext <- create_lower_extension(x)
  upper_ext <- create_upper_extension(x)

  c(lower_ext, sort(x), upper_ext)
}


