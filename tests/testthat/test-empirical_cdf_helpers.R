test_that("finding smaller index works for scalar", {
  val <- 4
  x <- 1:6
  i <- find_smaller_index(val, x)
  expect_equal(i, 4)
})

test_that("finding smaller index works for vector", {
  val <- c(3, 4)
  x <- 1:6
  i <- find_smaller_index(val, x)
  expect_equal(i, c(3, 4))
})

test_that("sample extension works", {
  samp <- 1:5
  xsamp <- extend_sample(samp)
  expect_equal(xsamp, 0:6)
})
