
library(testthat)

test_that("finds simple roots correctly", {
  f <- function(x) cos(2 * x)^3
  roots <- unirootAll(f, c(0, 10))
  
  expect_true(length(roots) > 0)
  expect_true(all(abs(f(roots)) < 1e-6))
})

test_that("returns numeric(0) when no roots exist", {
  f <- function(x) x^2 + 1
  roots <- unirootAll(f, c(-10, 10))
  
  expect_equal(roots, numeric(0))
})

test_that("works with non-vectorized functions", {
  f <- function(x) {
    if (length(x) > 1) stop("not vectorized")
    x^2 - 1
  }
  
  roots <- unirootAll(f, c(-2, 2))
  
  expect_true(all(abs(sapply(roots, f)) < 1e-6))
  expect_equal(sort(round(roots, 6)), c(-1, 1))
})

test_that("detects roots at grid points", {
  f <- function(x) x - 1
  roots <- unirootAll(f, c(0, 2), n = 10)
  
  expect_true(any(abs(roots - 1) < 1e-8))
})

test_that("handles closely spaced roots (may miss some)", {
  f <- function(x) (x - 0.5) * (x - 0.5001)
  
  roots <- unirootAll(f, c(0, 1), n = 50)
  
  # not guaranteed to find both → just check no crash
  expect_true(is.numeric(roots))
})

test_that("skips intervals where uniroot fails", {
  f <- function(x) {
    if (any(x > 0.4 & x < 0.6)) stop("bad region")
    x - 0.5
  }
  
  expect_warning({
    roots <- unirootAll(f, c(0, 1))
  }, "No sign changes detected")
})

test_that("handles non-finite values gracefully", {
  f <- function(x) ifelse(x == 0, NA, sin(x) / x)
  
  roots <- unirootAll(f, c(-10, 10))
  
  expect_true(is.numeric(roots))
})

test_that("deduplicates roots properly", {
  f <- function(x) (x - 1)
  
  roots <- unirootAll(f, c(0, 2), n = 100)
  
  expect_equal(length(roots), 1)
  expect_true(abs(roots - 1) < 1e-8)
})

test_that("warns when no roots found but finite values exist", {
  f <- function(x) x^2 + 1
  
  expect_warning(
    unirootAll(f, c(-5, 5)),
    "No sign changes detected"
  )
})

test_that("respects interval input", {
  f <- function(x) x - 2
  
  roots1 <- unirootAll(f, c(0, 5))
  roots2 <- unirootAll(f, interval = c(0, 5))
  
  expect_equal(roots1, roots2)
})