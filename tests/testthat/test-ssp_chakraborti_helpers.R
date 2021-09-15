test_that("numerical estimation of p runs", {
  x <- rnorm(10)
  delta <- 0.5
  p <- estimate_p(x, delta)

  expect_length(p, 1)
  expect_lte(p, 1)
  expect_gte(p, 0)
})


test_that("noether N estimation runs", {
  a <- 0.05
  pwr <- 0.9
  p <- 0.4

  n <- n_noether(a, pwr, p)
  expect_equal(n, 142.73079 * 2)
})


test_that("noether N increases with power", {
  a <- 0.05
  p <- 0.4
  pwr1 <- 0.8
  pwr2 <- 0.9

  n1 <- n_noether(a, pwr1, p)
  n2 <- n_noether(a, pwr2, p)

  expect_gt(n2, n1)
})


test_that("noether N increases with p approaching 0.5 from below", {
  a <- 0.05
  pwr <- 0.8
  p1 <- 0.3
  p2 <- 0.4
  p3 <- 0.45

  n1 <- n_noether(a, pwr, p1)
  n2 <- n_noether(a, pwr, p2)
  n3 <- n_noether(a, pwr, p3)

  expect_gt(n2, n1)
  expect_gt(n3, n1)
  expect_gt(n3, n2)
})


test_that("noether N increases with p approaching 0.5 from above", {
  a <- 0.05
  pwr <- 0.8
  p1 <- 0.7
  p2 <- 0.6
  p3 <- 0.55

  n1 <- n_noether(a, pwr, p1)
  n2 <- n_noether(a, pwr, p2)
  n3 <- n_noether(a, pwr, p3)

  expect_gt(n2, n1)
  expect_gt(n3, n1)
  expect_gt(n3, n2)
})


test_that("noether N is symmetric for p > 0.5 and p < 0.5", {
  a <- 0.05
  pwr <- 0.8
  p1 <- 0.3
  p2 <- 0.7

  n1 <- n_noether(a, pwr, p1)
  n2 <- n_noether(a, pwr, p2)

  expect_equal(n1, n2)
})


test_that("noether N decreses as alpha increases", {
  a1 <- 0.05
  a2 <- 0.1

  pwr <- 0.8
  p <- 0.3

  n1 <- n_noether(a1, pwr, p)
  n2 <- n_noether(a2, pwr, p)

  expect_gt(n1, n2)
})


test_that("N estimation runs", {
  a <- 0.05
  pwr <- 0.8
  x <- rnorm(10)
  delta <- 0.5

  n <- n_locshift_one(x, a, pwr, delta, q = 0.5)
  expect_length(n, 1)
})

test_that("N estimation increases with power", {
  a <- 0.05
  delta <- 1
  x <- rnorm(10)
  pwr1 <- 0.8
  pwr2 <- 0.9


  n1 <- n_locshift_one(x, a, pwr1, delta, q = 0.5)
  n2 <- n_locshift_one(x, a, pwr2, delta, q = 0.5)

  expect_gt(n2, n1)
})

test_that("N estimation increases with delta approaching 0 from below", {
  a <- 0.05
  pwr <- 0.8
  x <- rnorm(10)
  d1 <- 1
  d2 <- 0.5
  d3 <- 0.1



  n1 <- n_locshift_one(x, a, pwr, d1, q = 0.5)
  n2 <- n_locshift_one(x, a, pwr, d2, q = 0.5)
  n3 <- n_locshift_one(x, a, pwr, d3, q = 0.5)

  expect_gt(n2, n1)
  expect_gt(n3, n1)
  expect_gt(n3, n2)
})


test_that("N estimation increases with delta approaching 0 from above", {
  a <- 0.05
  pwr <- 0.8
  x <- rnorm(10)
  d1 <- -1
  d2 <- -0.5
  d3 <- -0.1



  n1 <- n_locshift_one(x, a, pwr, d1, q = 0.5)
  n2 <- n_locshift_one(x, a, pwr, d2, q = 0.5)
  n3 <- n_locshift_one(x, a, pwr, d3, q = 0.5)

  expect_gt(n2, n1)
  expect_gt(n3, n1)
  expect_gt(n3, n2)
})

test_that("N estimation is symmetric for delta > 0 and delta < 0", {
  a <- 0.05
  pwr <- 0.8
  x <- rnorm(10)
  d1 <- -1
  d2 <- -1



  n1 <- n_locshift_one(x, a, pwr, d1, q = 0.5)
  n2 <- n_locshift_one(x, a, pwr, d2, q = 0.5)

  expect_equal(n1, n2)
})


test_that("N estimation decreses as alpha increases", {
  a1 <- 0.05
  a2 <- 0.1
  x <- rnorm(10)

  pwr <- 0.8
  delta <- 0.3



  n1 <- n_locshift_one(x, a1, pwr, delta, q = 0.5)
  n2 <- n_locshift_one(x, a2, pwr, delta, q = 0.5)

  expect_gt(n1, n2)
})


test_that("resample N runs", {
  a <- 0.05
  pwr <- 0.8
  delta <- 0.6
  x <- rnorm(10)


  n_estimates <- resample_n_locshift_one(x, 50,
                                alpha = a,
                                power = pwr,
                                delta = delta,
                                q = 0.5)

  expect_length(n_estimates, 50)

  nas <- any(is.na(n_estimates))
  expect_false(nas)
})

