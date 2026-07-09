
# ── Tests for primes() ───────────────────────────────────────────────────────

library(testthat)

# ── return structure ──────────────────────────────────────────────────────────

test_that("single value returns an integer vector", {
  out <- primes(10)
  expect_type(out, "integer")
  expect_null(names(out))
})

test_that("several values return a named list", {
  out <- primes(c(5, 10, 20))
  expect_type(out, "list")
  expect_named(out, c("5", "10", "20"))
})

test_that("list length matches length of input", {
  out <- primes(c(5, 10, 20))
  expect_length(out, 3)
})

# ── known prime sets ──────────────────────────────────────────────────────────

test_that("primes up to 10 are correct", {
  expect_equal(primes(10), c(2L, 3L, 5L, 7L))
})

test_that("primes up to 2 returns only 2", {
  expect_equal(primes(2), 2L)
})

test_that("primes up to 20 are correct", {
  expect_equal(primes(20), c(2L, 3L, 5L, 7L, 11L, 13L, 17L, 19L))
})

# ── edge cases ────────────────────────────────────────────────────────────────

test_that("primes up to 1 returns empty vector", {
  expect_length(primes(1), 0)
})

test_that("primes rejects n < 1", {
  expect_error(primes(0), "positive whole numbers")
})

test_that("n equal to a prime includes that prime", {
  expect_true(7L %in% primes(7))
})

test_that("n equal to a composite excludes it", {
  expect_false(9L %in% primes(9))
})

# ── vectorisation ─────────────────────────────────────────────────────────────

test_that("vectorised call returns independent correct results", {
  out <- primes(c(5, 10))
  expect_equal(out[["5"]],  c(2L, 3L, 5L))
  expect_equal(out[["10"]], c(2L, 3L, 5L, 7L))
})

test_that("single-element input behaves like scalar call", {
  expect_equal(primes(10), primes(c(10)))
})

# ── result elements are integer vectors ───────────────────────────────────────

test_that("each list element is an integer vector", {
  out <- primes(c(20, 30))
  for (el in out)
    expect_type(el, "integer")
})

# ── primality spot checks ─────────────────────────────────────────────────────

test_that("all returned values are actually prime", {
  out <- primes(50)
  expect_true(all(vapply(out, isPrime, logical(1))))
})

test_that("no composite numbers appear in result up to 50", {
  composites <- c(4L, 6L, 8L, 9L, 10L, 12L, 14L, 15L, 16L, 18L,
                  20L, 21L, 22L, 24L, 25L, 26L, 27L, 28L)
  out <- primes(50)
  expect_true(!any(composites %in% out))
})
