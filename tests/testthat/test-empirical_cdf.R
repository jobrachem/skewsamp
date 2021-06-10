test_that("pemp works", {
  samp <- 1:4
  p <- pemp(samp, samp)
  expect_equal(p, c(0.2, 0.4, 0.6, 0.8))

  samp <- 0:6
  p <- pemp(samp, samp)
  control <- c(0.125, 0.250, 0.375, 0.500, 0.625, 0.750, 0.875)
  expect_equal(p, control)
})


test_that("pemp returns correct length", {
  samp <- 0:6
  p <- pemp(rnorm(10), samp)
  expect_length(p, 10)
})


test_that("demp runs works", {
  samp <- 1:4
  dens <- demp(1, samp)
  expect_equal(dens, 0.2)
})

test_that("demp returns correct length", {
  samp <- 1:4
  dens <- demp(rnorm(10), samp)
  expect_length(dens, 10)
})

test_that("qemp runs without error", {
  samp <- 1:4
  q <- qemp(0.2, samp)
  expect_equal(q, 1)
})

test_that("qemp returns correct length", {
  samp <- 1:4
  p <- seq(from = 0, to = 1, by = 0.2)
  q <- qemp(p, samp)
  expect_length(q, length(p))
})


test_that("remp runs without error", {
  samp <- 1:4
  x <- remp(1, samp)
  expect_length(x, 1)
  expect_gte(x, 0)
  expect_lte(x, 5)
})


test_that("remp returns correct length", {
  samp <- 1:4
  x <- remp(10, samp)
  expect_length(x, 10)
})
