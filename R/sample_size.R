new_sample_size <- function(n, two_sided, alpha, power, effect, effect_type, comment, q, call) {
  stopifnot(q < 1)
  stopifnot(q > 0)

  structure(
    list(
      n1 = n * q,
      n2 = n * (1 - q),
      n = n,
      two_sided = two_sided,
      alpha = alpha,
      power = power,
      effect = effect,
      effect_type = effect_type,
      comment = comment,
      call = call
    ),
    class = "sample_size"
  )
}

validate_sample_size <- function(sample_size) {
  stopifnot(sample_size$alpha < 1)
  stopifnot(sample_size$alpha > 0)
  stopifnot(sample_size$power < 1)
  stopifnot(sample_size$power > 0)

  sample_size
}


sample_size <- function(n, two_sided, alpha, power, effect, effect_type,
                        comment = "", q = 0.5, call = NULL) {
  validate_sample_size(
    new_sample_size(
      n, two_sided, alpha, power, effect, effect_type, comment, q, call
    )
  )
}


#' @export
print.sample_size <- function(x, ...) {
  cat("Estimated sample size for group difference.\n", x$comment, "\n\n")

  cat("N (total)\t\t", x$n, "\n")
  cat("N (Group 1)\t\t", x$n1, "\n")
  cat("N (Group 2)\t\t", x$n2, "\n\n")
  cat("Effect size\t\t", x$effect, "\n")
  cat("Effect type\t\t", x$effect_type, "\n")
  cat("Type I error\t\t", x$alpha, "\n")
  cat("Target power\t\t", x$power, "\n")
  cat("Two-sided\t\t", x$two_sided, "\n\n")
  if (!is.null(x$call)) {
    cat("Call:", paste(deparse(x$call), sep = "\n", collapse = "\n"))
  }
}

