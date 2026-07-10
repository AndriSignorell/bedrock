library(testthat)

# ── vRot ──────────────────────────────────────────────────────────────────────

test_that("vRot rotates right and left", {
  expect_equal(vRot(1:5, 2),  c(4L, 5L, 1L, 2L, 3L))
  expect_equal(vRot(1:5, -1), c(2L, 3L, 4L, 5L, 1L))
})

test_that("vRot wraps k beyond length", {
  expect_equal(vRot(1:5, 7), vRot(1:5, 2))
  expect_equal(vRot(1:5, 5), 1:5)
  expect_equal(vRot(1:5, 0), 1:5)
})

test_that("vRot handles empty input and preserves class", {
  expect_equal(vRot(integer(0)), integer(0))
  f <- factor(c("a", "b", "c"))
  expect_s3_class(vRot(f, 1), "factor")
  expect_equal(as.character(vRot(f, 1)), c("c", "a", "b"))
})

test_that("vRot warns on non-integer k and rejects invalid k", {
  expect_warning(res <- vRot(1:5, 1.6), "not an integer")
  expect_equal(res, vRot(1:5, 2))
  expect_error(vRot(1:5, NA), "single number")
  expect_error(vRot(1:5, 1:2), "single number")
})

# ── vShift ────────────────────────────────────────────────────────────────────

test_that("vShift shifts with NA padding", {
  expect_equal(vShift(1:5, 2),  c(NA, NA, 1L, 2L, 3L))
  expect_equal(vShift(1:5, -2), c(3L, 4L, 5L, NA, NA))
})

test_that("vShift discards everything when k >= n", {
  expect_equal(vShift(1:5, 10), rep(NA_integer_, 5))
  expect_equal(vShift(1:5, -10), rep(NA_integer_, 5))
})

test_that("vShift preserves factor class", {
  # regression: c(NA, factor) dispatched on the logical NA and
  # returned integer codes
  f <- factor(c("a", "b", "c"))
  res <- vShift(f, 1)
  expect_s3_class(res, "factor")
  expect_equal(as.character(res), c(NA, "a", "b"))
})

test_that("vShift preserves Date class", {
  d <- as.Date("2026-01-01") + 0:2
  res <- vShift(d, -1)
  expect_s3_class(res, "Date")
  expect_equal(res, c(d[2:3], as.Date(NA)))
})

test_that("vShift rejects invalid k", {
  expect_error(vShift(1:5, NA), "single number")
  expect_error(vShift(1:5, "a"), "single number")
})
