test_that("n_gamma works, effect = 0.3", {
    n <- n_gamma(8.46, 0.3, 0.639)
    n <- ceiling(n$n)

    expect_equal(n, 518)
})

test_that("n_gamma works, effect = 0.5", {
  n <- n_gamma(8.46, 0.5, 0.639)
  n <- ceiling(n$n)

  expect_lt(n, 146)
  expect_gt(n, 129)
})

test_that("n_gamma works, effect = 0.7", {
  n <- n_gamma(8.46, 0.7, 0.639)
  n <- ceiling(n$n)

  expect_equal(n, 46)
})


test_that("n_negbinom works, effect = 0.3", {
  n <- n_negbinom(71.4, 0.3, 0.33)

  expect_equal(n$n, 1006.76387)
})

test_that("n_negbinom works, effect = 0.7", {
  n <- n_negbinom(71.4, 0.7, 0.33)

  expect_equal(n$n, 88.74345)
})


test_that("n_binom works, effect = 0.3", {
  n <- n_binom(0.5, 0.3)

  expect_equal(round(n$n, 3), 1342.750)
})

test_that("n_binom works, effect = 0.7", {
  n <- n_binom(0.5, 0.7)

  expect_equal(round(n$n, 3), round(139.65917, 3))
})

test_that("n_poisson works, effect = 0.3", {
  n <- n_poisson(2, 0.3)

  expect_equal(round(n$n, 3), 200.587)
})

test_that("n_poisson works, effect = 0.7", {
  n <- n_poisson(2, 0.7)

  expect_equal(round(n$n, 3), 31.411)
})
