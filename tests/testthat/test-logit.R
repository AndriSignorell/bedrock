
library(testthat)

## --- basic correctness ---

test_that("logit and inverse are consistent on (min, max)", {
  x <- seq(0.01, 0.99, length.out = 10)
  z <- logit(x)
  x_back <- logitInv(z)
  
  expect_equal(x_back, x, tolerance = 1e-10)
})

test_that("works on custom interval", {
  x <- seq(10.1, 19.9, length.out = 10)
  z <- logit(x, min = 10, max = 20)
  x_back <- logitInv(z, min = 10, max = 20)
  
  expect_equal(x_back, x, tolerance = 1e-10)
})

## --- clamping behavior ---

test_that("logit clamps boundary values", {
  z <- logit(c(0, 1))
  
  expect_true(all(is.finite(z)))
  expect_true(z[1] < 0)  # near -Inf
  expect_true(z[2] > 0)  # near +Inf
})

test_that("logit clamps values outside interval", {
  z <- logit(c(-0.1, 1.1))
  
  expect_true(all(is.finite(z)))
})

## --- warning behavior ---

test_that("warn = TRUE triggers warning at boundaries", {
  expect_warning(
    logit(c(0, 0.5, 1), warn = TRUE),
    "clamped"
  )
})

test_that("warn = TRUE triggers warning outside interval", {
  expect_warning(
    logit(c(-0.1, 0.5, 1.1), warn = TRUE),
    "clamped"
  )
})

test_that("no warning when values strictly inside interval", {
  expect_silent(
    logit(c(0.1, 0.5, 0.9), warn = TRUE)
  )
})

## --- input validation ---

test_that("non-numeric input errors", {
  expect_error(logit("a"))
  expect_error(logitInv("a"))
})

test_that("invalid interval errors", {
  expect_error(logit(0.5, min = 1, max = 1))
  expect_error(logitInv(0, min = 2, max = 1))
})

## --- numerical properties ---

test_that("logit is monotonic increasing", {
  x <- seq(0.01, 0.99, length.out = 100)
  z <- logit(x)
  
  expect_true(all(diff(z) > 0))
})

test_that("logitInv maps to [min, max]", {
  z <- seq(-10, 10, length.out = 100)
  x <- logitInv(z)
  
  expect_true(all(x >= 0))
  expect_true(all(x <= 1))
})

## --- NA handling ---

test_that("NA values propagate", {
  x <- c(0.2, NA, 0.8)
  z <- logit(x)
  
  expect_true(is.na(z[2]))
})

test_that("logitInv handles NA", {
  z <- c(-1, NA, 1)
  x <- logitInv(z)
  
  expect_true(is.na(x[2]))
})

## --- extreme values ---

test_that("large values in logitInv are stable", {
  z <- c(-100, 0, 100)
  x <- logitInv(z)
  
  expect_true(all(is.finite(x)))
  expect_true(x[1] < 0.01)
  expect_true(x[3] > 0.99)
})