# For debugging the numerical integration in estimate_one_p
debug_integration <- function(combined_sample, i, h, sample, shifted_sample, cond) {
  cat("\nError in integral from ")
  cat(combined_sample[i])
  cat(" to ")
  cat(combined_sample[i + 1])
  cat("\n")
  message(cond)
  cat("\n\n")

  y <- h(combined_sample)

  par(mfrow=c(1, 3))
  plot(combined_sample, y, type = "l", main="Integral function")
  abline(v = csamp[i], col = "red")
  abline(v = csamp[i+1], col = "red")

  plot(combined_sample, pemp(combined_sample, shifted_sample), type = "l", main = "CDF")

  plot(combined_sample, demp(combined_sample, sample), type = "l", main = "PDF")
  par(mfrow=c(1, 1))
}
