
# ── precision ─────────────────────────────────────────────────────────────────

test_that("frac returns fractional part", {
  expect_equal(frac(3.75), 0.75)
  expect_equal(frac(-2.5), 0.5)
  expect_equal(frac(4.0), 0.0)
})

test_that("frac with dpwr multiplies", {
  expect_equal(frac(3.75, dpwr = 2), 75)
})

test_that("maxDigits returns max decimal places", {
  expect_equal(maxDigits(c(1.25, 1.8, 12.0)), 2L)
  expect_equal(maxDigits(c(1, 2, 3)), 0L)
})

test_that("nDec returns number of decimal places", {
  x <- c("0.0000", "0", "159.283")
  res <- nDec(x)
  expect_equal(res, c(4L, 0L, 3L))
})

test_that("prec returns correct precision", {
  expect_equal(prec(1.235), 0.001)
  expect_equal(prec(125.3), 0.1)
})


