library(testthat)

test_that("values are rounded to the nearest multiple", {
  expect_equal(roundTo(10, 3), 9)
  expect_equal(roundTo(-10, 3), -9)
  expect_equal(roundTo(1.3, 0.2), 1.2)
  expect_equal(roundTo(-1.3, 0.2), -1.2)
})

test_that("the default multiple is 1", {
  expect_equal(roundTo(c(1.4, 1.6, -1.4)), c(1, 2, -1))
})

test_that("ties are resolved to the even multiple", {
  expect_equal(roundTo(c(1, 3, 5, 7), 2), c(0, 4, 4, 8))
})

test_that("FUN controls the direction of the rounding", {
  x <- c(1, -1) * 1.2335
  expect_equal(roundTo(x, 0.05, floor),   c(1.20, -1.25))
  expect_equal(roundTo(x, 0.05, ceiling), c(1.25, -1.20))
  expect_equal(roundTo(x, 0.05, trunc),   c(1.20, -1.20))
  expect_equal(roundTo(x, 0.05, round),   c(1.25, -1.25))
})

test_that("any function of a numeric vector can be used", {
  expect_equal(roundTo(c(2.2, 2.8), 1, FUN = function(z) z^0 * 3), c(3, 3))
})

test_that("a scalar multiple applies to all values", {
  expect_equal(roundTo(c(1.02, 1.03, 12.375), 0.05), c(1.00, 1.05, 12.40))
})

test_that("a multiple as long as x is used elementwise", {
  expect_equal(roundTo(c(1.23, 123, 1234), c(0.05, 10, 100)),
               c(1.25, 120, 1200))
})

test_that("a deviating length is an error, not recycled", {
  expect_error(roundTo(1:6, c(2, 3)), "must be 1 or the length")
  expect_error(roundTo(1:5, c(2, 3)), "must be 1 or the length")
  expect_error(roundTo(1:3, numeric(0)), "must be 1 or the length")
})

test_that("multiple must be finite and positive", {
  expect_error(roundTo(10, 0), "finite and positive")
  expect_error(roundTo(10, -3), "finite and positive")
  expect_error(roundTo(-10, -3), "finite and positive")
  expect_error(roundTo(10, NA), "finite and positive")
  expect_error(roundTo(10, Inf), "finite and positive")
  expect_error(roundTo(c(1, 2), c(1, -2)), "finite and positive")
})

test_that("FUN must be a function", {
  expect_error(roundTo(10, 3, "floor"), "must be a function")
  expect_error(roundTo(10, 3, FUN = NULL), "must be a function")
})

test_that("NAs in x are returned as NA", {
  expect_equal(roundTo(c(1.3, NA), 0.2), c(1.2, NA))
})

test_that("a zero length x returns a zero length result", {
  expect_equal(roundTo(numeric(0), 2), numeric(0))
  expect_equal(roundTo(numeric(0), numeric(0)), numeric(0))
})

test_that("the result is as long as x", {
  expect_length(roundTo(1:7, 2), 7)
  expect_length(roundTo(1:7, rep(2, 7)), 7)
})



test_that("roundTo supports all documented rounding directions", {
  x <- c(-1.26, 1.26)
  
  expect_equal(roundTo(x, 0.1, FUN = floor), c(-1.3, 1.2))
  expect_equal(roundTo(x, 0.1, FUN = ceiling), c(-1.2, 1.3))
  expect_equal(roundTo(x, 0.1, FUN = trunc), c(-1.2, 1.2))
})

test_that("roundTo follows round-to-even at exact ties", {
  expect_equal(roundTo(c(2.5, 3.5), 1), c(2, 4))
})

test_that("roundTo rejects zero, negative and missing multiples", {
  expect_error(roundTo(1, 0), "finite and positive")
  expect_error(roundTo(1, -2), "finite and positive")
  expect_error(roundTo(1, NA_real_), "finite and positive")
})


