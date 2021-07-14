test_that("n_gamma works, efficacy = 0.3", {
    n <- n_gamma(8.46, 0.3, 0.639)
    n <- ceiling(n$n)

    expect_equal(n, 518)
})

test_that("n_gamma works, efficacy = 0.5", {
  n <- n_gamma(8.46, 0.5, 0.639)
  n <- ceiling(n$n)

  expect_lt(n, 146)
  expect_gt(n, 129)
})

test_that("n_gamma works, efficacy = 0.7", {
  n <- n_gamma(8.46, 0.7, 0.639)
  n <- ceiling(n$n)

  expect_equal(n, 46)
})


test_that("n_negbinom works, efficacy = 0.3", {
  n <- n_negbinom(71.4, 0.3, 0.33)

  expect_equal(n$n, 1006.76387)
})

test_that("n_negbinom works, efficacy = 0.7", {
  n <- n_negbinom(71.4, 0.7, 0.33)

  expect_equal(n$n, 88.74345)
})

#
#
#
# plot_gamma <- function(mean, var, add = FALSE) {
#   shape <- get_shape(mean, var)
#   scale <- get_scale(mean, var)
#
#   x <- seq(from = 0, to = 5, length.out = 101)
#   d <- dgamma(x, shape = shape, scale = scale)
#   if (add) {
#     lines(x, d, type = "l", col = "red")
#   } else {
#     plot(x, d, type = "l")
#   }
# }
#
# plot_gamma(1, 1)
# plot_gamma(2, 1, add = TRUE)
#
#
# sim_gamma <- function(n, mean, var) {
#   shape <- get_shape(mean, var)
#   scale <- get_scale(mean, var)
#
#   rgamma(n, shape = shape, scale = scale)
# }
#
# n <- 100
#
#
# sim_df <- function(n, mean1, mean2, var1, var2) {
#   y1 <- sim_gamma(n, mean1, var1)
#   y2 <- sim_gamma(n, mean2, var2)
#
#   df <- data.frame(y = c(y1, y2), x = rep(c(0, 1), each = n))
#   df
# }
#
#
# eval_model <- function(n, mean1, mean2, var1 = 1, var2 = 1, alpha = 0.05) {
#
#   df <- sim_df(n, mean1, mean2, var1, var2)
#
#   m <- glm(y ~ x, data = df, family = Gamma(link = "log"))
#   m_summary <- summary(m)
#   p <- coef(m_summary)[2,4]
#
#   p <= alpha
# }
#
# eval_t_test <- function(n, mean1, mean2, var1 = 1, var2 = 1, alpha = 0.05) {
#   df <- sim_df(n, mean1, mean2, var1, var2)
#
#   t_test <- t.test(y ~ x, data = df)
#   t_test$p.value <= alpha
# }
#
#
# sim_study <- function(nsim, fun, ...) {
#   p <- vector(mode = "numeric", length=nsim)
#   for (i in seq(nsim)) {
#     p[i] <- fun(...)
#   }
#   pwr <- sum(p) / nsim
#   pwr
# }
#
# sim_study(nsim = 500, eval_t_test, n = 220, mean1 = 1, mean2 = 2)
# sim_study(nsim = 500, eval_model, n = 220, mean1 = 1, mean2 = 2)
#
# x <- seq(from = -1, to = 2, by = 0.1)
# plot(x, exp(x), type = "l")
#
# exp(0)
# exp(1)
# exp(-0.5)
#
#
# sim_df2 <- function(efficacy, mean, shape, n) {
#   scale0 <- mean / shape
#   scale1 <- scale0 * (1 - efficacy)
#   print(shape)
#   print(scale0)
#   print(scale1)
#   y0 <- rgamma(ceiling(n / 2), shape = shape, scale = scale0)
#   y1 <- rgamma(ceiling(n / 2), shape = shape, scale = scale1)
#
#   x <- rep(c(0, 1), each = ceiling(n / 2))
#   df <- data.frame(y = c(y0, y1), x = x)
#   df
# }
#
#
# eval_model <- function(df, alpha = 0.05) {
#   m <- glm(y ~ x, data = df, family = Gamma(link = "log"))
#   m_summary <- summary(m)
#   p <- coef(m_summary)[2,4]
#
#   p <= alpha
# }
#
# eval_wmu <- function(df, alpha = 0.05) {
#   m <- wilcox.test(y ~ x, data = df)
#   p <- m$p.value
#
#   p <= alpha
# }
#
#
# nsim <- 1000
# p <- vector(mode = "numeric", length=nsim)
# p2 <- vector(mode = "numeric", length=nsim)
# for (i in seq(nsim)) {
#   df <- sim_df2(0.7, 8.46, 0.639, 46)
#   p[i] <- eval_model(df)
#   p2[i] <- eval_wmu(df)
# }
# pwr <- sum(p) / nsim
# pwr
#
# pwr2 <- sum(p2) / nsim
# pwr2
#
# df <- sim_df2(0.7, 8.46, 0.639, 46)
# y0 <- df[df$x == 0,]$y
# y1 <- df[df$x == 1,]$y
#
# par(mfrow = c(1, 2))
# hist(y0)
# hist(y1)
#
# plot(density(y0), ylim = c(0, 0.3))
# lines(density(y1), col = "red")
#
# median(y0)
# median(y1)
#
# df <- sim_df2(efficacy = 0.7, mean = 8.46, shape = 0.639, n = 46)
#
#
#
#
# rnorm(10)
# rnorm(10) + 0.5
#
# rnorm(10)
# rnorm(10, 0.5)
#
# rgamma(10)
# rgamma(10) + 0.5
