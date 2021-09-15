test_that("n_locshift runs", {
  n <- n_locshift(rnorm(10), rnorm(10), delta = 0.3)

  expect_s3_class(n, "sample_size")
  expect_equal(n$n0, n$n1)
  expect_equal(n$n, n$n0 + n$n1)
})

test_that("q parameter works", {
  n <- n_locshift(rnorm(10), rnorm(10), delta = 0.3, q = 0.3)

  expect_lt(n$n0, n$n1)
})

test_that("resample_n_locshift runs", {
  n <- resample_n_locshift(rnorm(10), rnorm(10), delta = 0.3, n_resamples = 100)

  expect_length(n, 100)
  expect_gt(min(n), 0)
})

test_that("n_locshift_bound runs", {
  set.seed(1)
  s1 <- rnorm(10)
  s2 <- rnorm(10)

  n <- n_locshift(s1, s2, delta = 0.3)
  bound <- n_locshift_bound(s1, s2, delta = 0.3, n_resamples = 100)

  expect_gt(bound$n, n$n)
})
