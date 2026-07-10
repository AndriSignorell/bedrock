library(testthat)

test_that("winsorize clips at fixed bounds", {
  x <- c(-100, 1, 2, 3, 100)
  expect_equal(winsorize(x, val = c(0, 10)), c(0, 1, 2, 3, 10))
})

test_that("winsorize preserves NAs", {
  x <- c(1, NA, 100)
  res <- winsorize(x, val = c(0, 10))
  expect_equal(res, c(1, NA, 10))
})

test_that("default quantile bounds clip extremes", {
  set.seed(9128)
  x <- c(rnorm(100), -100, 100)
  res <- winsorize(x)
  q <- quantile(x, c(0.05, 0.95), na.rm = TRUE)
  expect_true(all(res >= q[1] & res <= q[2]))
})

test_that("one-sided winsorization via Inf", {
  x <- c(-100, 0, 100)
  expect_equal(winsorize(x, val = c(-Inf, 2)), c(-100, 0, 2))
  expect_equal(winsorize(x, val = c(-2, Inf)), c(-2, 0, 100))
})

test_that("invalid val is rejected", {
  expect_error(winsorize(1:10, val = 1),          "length 2")
  expect_error(winsorize(1:10, val = c(1, NA)),   "length 2")
  expect_error(winsorize(1:10, val = c(10, 1)),   "must not exceed")
  expect_error(winsorize(1:10, val = c("a", "b")), "numeric")
})
