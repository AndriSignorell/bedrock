
# ── Tests for isZero() and nz() ──────────────────────────────────────────────

library(testthat)

tol <- sqrt(.Machine$double.eps)

# ── isZero: exact zero ───────────────────────────────────────────────────────

test_that("isZero returns TRUE for exact zero", {
  expect_true(isZero(0))
})

test_that("isZero returns TRUE for vector of zeros", {
  expect_equal(isZero(c(0, 0, 0)), c(TRUE, TRUE, TRUE))
})

# ── isZero: floating point zero ──────────────────────────────────────────────

test_that("isZero returns TRUE for floating point near-zero (.1 - .3/3)", {
  expect_true(isZero(.1 - .3 / 3))
})

test_that("isZero returns FALSE for value just above tolerance", {
  expect_false(isZero(tol * 2))
})

test_that("isZero returns TRUE for value just below tolerance", {
  expect_true(isZero(tol / 2))
})

# ── isZero: mixed vector ─────────────────────────────────────────────────────

test_that("isZero handles mixed zero/nonzero vector correctly", {
  expect_equal(isZero(c(0, 1, -1, .1 - .3 / 3, 0.5)),
               c(TRUE, FALSE, FALSE, TRUE, FALSE))
})

# ── isZero: non-numeric input ────────────────────────────────────────────────

test_that("isZero returns FALSE for character input", {
  expect_false(isZero("0"))
})

test_that("isZero returns FALSE for logical input", {
  expect_false(isZero(FALSE))
})

# ── isZero: NA handling ──────────────────────────────────────────────────────

test_that("isZero propagates NA by default (na.rm = FALSE)", {
  result <- isZero(c(0, NA, 1))
  expect_equal(result, c(TRUE, NA, FALSE))
})

test_that("isZero strips NA when na.rm = TRUE", {
  result <- isZero(c(0, NA, 1), na.rm = TRUE)
  expect_equal(result, c(TRUE, FALSE))   # NA removed, result is shorter
})

# ── isZero: custom tolerance ─────────────────────────────────────────────────

test_that("isZero respects custom tolerance", {
  expect_true(isZero(0.01, tol = 0.1))
  expect_false(isZero(0.01, tol = 0.001))
})

# ── isZero: return type ──────────────────────────────────────────────────────

test_that("isZero returns a logical vector of same length as input", {
  x <- c(0, 1, 2, 0, -1)
  result <- isZero(x)
  expect_type(result, "logical")
  expect_length(result, length(x))
})

# ── nz: basic filtering ──────────────────────────────────────────────────────

test_that("nz removes exact zeros", {
  expect_equal(nz(c(0, 1, 2, 0, 3)), c(1, 2, 3))
})

test_that("nz removes floating point near-zeros", {
  x <- c(.1 - .3 / 3, 1, 2)
  expect_equal(nz(x), c(1, 2))
})

test_that("nz returns empty vector when all elements are zero", {
  expect_equal(nz(c(0, 0, 0)), numeric(0))
})

test_that("nz returns full vector when no elements are zero", {
  x <- c(1, 2, 3)
  expect_equal(nz(x), x)
})

# ── nz: NA behaviour (inherited from isZero) ─────────────────────────────────

test_that("nz retains NA values (isZero default na.rm = FALSE)", {
  # NA is neither zero nor nonzero: !NA = NA, so x[NA] returns NA
  result <- nz(c(1, NA, 0, 2))
  expect_equal(result, c(1, NA, 2))
})

# ── nz: return type ──────────────────────────────────────────────────────────

test_that("nz returns a numeric vector", {
  expect_type(nz(c(0, 1, 2)), "double")
})

test_that("nz preserves names", {
  x <- c(a = 0, b = 1, c = 2)
  expect_equal(nz(x), c(b = 1, c = 2))
})