
# ============================================================
# test-closest.R
# ============================================================

# -----------------------------------------------------------------------
# Basic value return
# -----------------------------------------------------------------------

test_that("returns exact match when present", {
  expect_equal(closest(1:5, 3), 3)
})


test_that("returns closest value when no exact match", {
  expect_equal(closest(c(1, 2, 4, 5), 3), c(2, 4))  # tie at distance 1
})


test_that("returns single closest value", {
  expect_equal(closest(c(1, 2, 4, 5), 3.1), 4)
})


test_that("returns all duplicates of the closest value", {
  x   <- c(3, 5, 5, 8)
  res <- closest(x, 5)
  expect_equal(sort(res), c(5, 5))
})


test_that("returns both tied values at equal distance", {
  res <- closest(c(2, 3, 4, 5), 3.5)
  expect_equal(sort(res), c(3, 4))
})


test_that("single element vector returns that element", {
  expect_equal(closest(7, 3), 7)
})


test_that("a equals minimum of x", {
  expect_equal(closest(1:10, 1), 1)
})


test_that("a equals maximum of x", {
  expect_equal(closest(1:10, 10), 10)
})


test_that("a below range returns minimum", {
  expect_equal(closest(3:7, -100), 3)
})


test_that("a above range returns maximum", {
  expect_equal(closest(3:7, 100), 7)
})


# -----------------------------------------------------------------------
# Index return
# -----------------------------------------------------------------------

test_that("idx = TRUE returns position", {
  expect_equal(closest(c(10, 20, 30), 20, idx = TRUE), 2L)
})


test_that("idx = TRUE returns all tied positions", {
  x   <- c(5, 5, 9)
  res <- closest(x, 5, idx = TRUE)
  expect_equal(sort(res), c(1L, 2L))
})


test_that("idx = TRUE returns integer vector", {
  res <- closest(1:5, 3, idx = TRUE)
  expect_type(res, "integer")
})


# -----------------------------------------------------------------------
# na.rm and index preservation
# -----------------------------------------------------------------------

test_that("na.rm = FALSE returns NA when x is all-NA", {
  expect_true(all(is.na(closest(c(NA_real_, NA_real_), 5))))
})


test_that("na.rm = FALSE: NA in x propagates, result contains NA", {
  res <- closest(c(NA_real_, 5, 8), 6)
  expect_true(anyNA(res))
})


test_that("na.rm = TRUE ignores NA and finds correct value", {
  expect_equal(closest(c(NA, 5, 8), 6, na.rm = TRUE), 5)
})


test_that("na.rm = TRUE preserves original index positions", {
  # x[1] = NA, x[2] = 5, x[3] = 8 — closest to 6 is x[2]
  res <- closest(c(NA, 5, 8), 6, idx = TRUE, na.rm = TRUE)
  expect_equal(res, 2L)   # NOT 1
})


test_that("na.rm = TRUE with all-NA returns NA", {
  expect_true(is.na(closest(c(NA_real_, NA_real_), 5, na.rm = TRUE)))
})


test_that("na.rm = TRUE with leading NAs preserves correct indices", {
  x   <- c(NA, NA, 1, 5, 9)
  res <- closest(x, 4, idx = TRUE, na.rm = TRUE)
  expect_equal(res, 4L)   # x[4] = 5
})


# -----------------------------------------------------------------------
# Empty and edge-case x
# -----------------------------------------------------------------------

test_that("empty x returns NA", {
  expect_true(is.na(closest(numeric(0), 5)))
})


test_that("empty x with idx = TRUE returns NA", {
  expect_true(is.na(closest(numeric(0), 5, idx = TRUE)))
})


# -----------------------------------------------------------------------
# Floating-point safety
# -----------------------------------------------------------------------

test_that("floating-point tie detected correctly", {
  # 0.1 + 0.2 != 0.3 in IEEE 754; isZero should handle this
  x   <- c(0.1 + 0.2, 0.4)   # first element ≈ 0.3
  res <- closest(x, 0.3)
  expect_length(res, 1L)
  expect_equal(res, 0.1 + 0.2, tolerance = 1e-10)
})


test_that("tie at equal floating-point distance returns both", {
  x   <- c(1.0, 1.4, 1.6, 2.0)
  res <- closest(x, 1.5)
  expect_equal(sort(res), c(1.4, 1.6))
})


# -----------------------------------------------------------------------
# Vectorized a
# -----------------------------------------------------------------------

test_that("vectorized a returns list", {
  res <- closest(1:5, c(1.9, 3.1))
  expect_type(res, "list")
  expect_length(res, 2L)
})


test_that("vectorized a: each element correct", {
  res <- closest(1:5, c(1.9, 3.1))
  expect_equal(res[[1L]], 2)
  expect_equal(res[[2L]], 3)
})


test_that("scalar a returns vector, not list", {
  res <- closest(1:5, 3)
  expect_false(is.list(res))
})


# -----------------------------------------------------------------------
# Vectorized return.index
# -----------------------------------------------------------------------

test_that("vectorized return.index recycled over a", {
  res <- closest(c(2, 3, 4, 5), a = c(3.1, 3.9),
                 idx = c(FALSE, TRUE))
  expect_equal(res[[1L]], 3)    # value
  expect_equal(res[[2L]], 3L)   # index of 4
})


test_that("return.index vectorized over scalar a returns list", {
  res <- closest(c(2, 3, 4, 5), a = 3.1,
                 idx = c(FALSE, TRUE))
  expect_type(res, "list")
  expect_equal(res[[1L]], 3)
  expect_equal(res[[2L]], 2L)
})


# -----------------------------------------------------------------------
# Input validation
# -----------------------------------------------------------------------

test_that("non-numeric x raises error", {
  expect_error(closest(letters[1:5], 3), "'x' must be numeric")
})


test_that("non-numeric a raises error", {
  expect_error(closest(1:5, "3"), "'a' must be numeric")
})
