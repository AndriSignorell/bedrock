
# ── precision ─────────────────────────────────────────────────────────────────

test_that("frac returns fractional part", {
  expect_equal(frac(3.75), 0.75)
  expect_equal(frac(-2.5), 0.5)
  expect_equal(frac(4.0), 0.0)
})

test_that("frac discards the sign", {
  # the sign belongs to the integer part
  expect_equal(frac(c(-1.25, 1.25)), c(0.25, 0.25))
})

test_that("frac keeps missing values and rejects non-numerics", {
  expect_equal(frac(c(1.5, NA)), c(0.5, NA))
  expect_equal(frac(numeric(0)), numeric(0))
  expect_error(frac("1.5"), "must be numeric")
})

test_that("the decimals are read as an integer by scaling", {
  # the former dpwr argument, spelled out
  expect_equal(round(1e2 * frac(3.75)), 75)
})

# ── nDec ──────────────────────────────────────────────────────────────────────

test_that("nDec returns number of decimal places", {
  x <- c("0.0000", "0", "159.283")
  res <- nDec(x)
  expect_equal(res, c(4L, 0L, 3L))
})

test_that("nDec counts what is printed, not what was typed", {
  # trailing zeros of a numeric are gone before counting
  expect_equal(nDec(1.500), 1L)
  # but they survive in a character input
  expect_equal(nDec("1.500"), 3L)
  # and a number printed in scientific notation has no decimals; where R
  # switches is R's decision and has moved between versions, so the test
  # uses a value that is certainly beyond it
  expect_equal(nDec(1e-300), 0L)
})

test_that("nDec counts a numeric exactly as its printed form", {
  # whether R writes 0.00001 in full or as 1e-05 depends on the R version,
  # and before R 4.3 also on options(scipen=); the invariant is that nDec()
  # counts whatever as.character() produced, not a fixed number of digits
  for (value in c(0.00001, 1e-300, 1.5, 1234.5678, 1e6))
    expect_equal(nDec(value), nDec(as.character(value)))
})

test_that("nDec ignores the exponent", {
  expect_equal(nDec(c("1.45e+10", "1.4599E+10")), c(2L, 4L))
})

test_that("nDec takes the last separator, comma or period", {
  expect_equal(nDec("159,283"), 3L)
  # a thousands separator must not distort the count
  expect_equal(nDec(c("1,234.56", "1.234,56")), c(2L, 2L))
})

test_that("nDec counts numeric input whatever OutDec says", {
  # as.character() writes a period regardless of the option, so deriving the
  # separator from format() used to make every count come out as zero
  op <- options(OutDec = ",")
  res <- nDec(c(1.5, 159.283))
  options(op)

  expect_equal(res, c(1L, 3L))
})

test_that("nDec passes missing values through", {
  expect_identical(nDec(c(1.5, NA)), c(1L, NA_integer_))
  expect_identical(nDec(NA), NA_integer_)
  expect_identical(nDec(numeric(0)), integer(0))
})

# ── maxDec ────────────────────────────────────────────────────────────────────

test_that("maxDec returns max decimal places", {
  expect_equal(maxDec(c(1.25, 1.8, 12.0)), 2L)
  expect_equal(maxDec(c(1, 2, 3)), 0L)
})

test_that("maxDec is the maximum of nDec", {
  x <- c("0.0000", "0", "159.283", "1.45e+10")
  expect_identical(maxDec(x), max(nDec(x)))
})

test_that("maxDec returns 0 when there is nothing to count", {
  expect_identical(maxDec(c(NA, NA)), 0L)
  expect_identical(maxDec(numeric(0)), 0L)
})

test_that("maxDec ignores the exponent as nDec does", {
  # the old implementation counted the exponent characters and returned 6
  expect_identical(maxDec("1.45e+10"), 2L)
})

# ── prec ──────────────────────────────────────────────────────────────────────

test_that("prec returns correct precision", {
  expect_equal(prec(1.235), 0.001)
  expect_equal(prec(125.3), 0.1)
})

test_that("prec reports one value for the whole vector", {
  # the finest precision found anywhere in x
  expect_equal(prec(c(1.5, 2.25, 3)), 0.01)
})

test_that("prec handles the degenerate cases", {
  expect_equal(prec(0), 1)
  expect_equal(prec(c(0, 0)), 1)
  expect_identical(prec(numeric(0)), NA_real_)
  expect_error(prec("1.5"), "must be numeric")
})

test_that("an all-NA vector is accepted although it is logical", {
  # c(NA, NA) is logical, and rejecting it would refuse an input the
  # functions document a result for
  expect_identical(prec(c(NA, NA)), NA_real_)
  expect_identical(prec(NA), NA_real_)

  expect_identical(maxDec(c(NA, NA)), 0L)
  expect_identical(nDec(c(NA, NA)), c(NA_integer_, NA_integer_))
  expect_true(all(is.na(frac(c(NA, NA)))))
})

test_that("prec ignores the sign", {
  expect_equal(prec(-1.235), 0.001)
  expect_equal(prec(c(-1.5, 1.5)), 0.1)
})

test_that("prec matches maxDec for input that is exact in decimal", {
  x <- c(1.25, 1.8, 12)
  expect_equal(prec(x), 10^-maxDec(x))
})
