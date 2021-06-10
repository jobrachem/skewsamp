estimate_n_chak <- function(x, y, ...) {
  nx <- estimate_one_n(x, ...)
  ny <- estimate_one_n(y, ...)

  ceiling(mean(c(nx, ny)))
}

resample_n_chak <- function(x, y, ...) {
  resamples_x <- resample_one_n(x, ...)
  resamples_y <- resample_one_n(x, ...)

  resamples <- ceiling((resamples_x + resamples_y) / 2)
  resamples
}

