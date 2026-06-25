
# ------------------------------------------------------------------------------
# GCD
# ------------------------------------------------------------------------------

test_that("GCD basic pairs", {
  expect_equal(GCD(12, 10),   2)
  expect_equal(GCD(12, 8),    4)
  expect_equal(GCD(7,  13),   1)   # coprime
  expect_equal(GCD(144, 233), 1)   # Fibonacci pair
  expect_equal(GCD(100, 75),  25)
})

test_that("GCD more than two values", {
  expect_equal(GCD(c(12, 8, 4)),      4)
  expect_equal(GCD(c(2*3, 3*5, 5*7)), 1)
  expect_equal(GCD(2, 3, c(5, 7) * 11), 1)
  expect_equal(GCD(6, 12, 18),        6)
})

test_that("GCD with zeros in input (zeros stripped)", {
  expect_equal(GCD(0, 0, 6), 6)   # all zeros except one
  expect_equal(GCD(0, 0, 0, 4), 4)
})

test_that("GCD with negatives", {
  # mathematical GCD is defined on absolute values
  expect_equal(GCD(-12, 8), GCD(12, 8))
})

test_that("GCD na.rm behaviour", {
  expect_true(is.na(GCD(12, NA, 8)))
  expect_equal(GCD(12, NA, 8, na.rm = TRUE), 4)
})

test_that("GCD errors on non-integer input", {
  expect_error(GCD(1.5, 2))
  expect_error(GCD(1))           # length < 2
})

test_that("GCD identity: n * m == GCD(n,m) * LCM(n,m)", {
  for (pair in list(c(12, 10), c(7, 13), c(144, 233), c(36, 84))) {
    n <- pair[1]; m <- pair[2]
    expect_equal(n * m, GCD(n, m) * LCM(n, m))
  }
})


# ------------------------------------------------------------------------------
# LCM
# ------------------------------------------------------------------------------

test_that("LCM basic pairs", {
  expect_equal(LCM(12, 10),    60)
  expect_equal(LCM(4, 6),      12)
  expect_equal(LCM(7, 13),     91)   # coprime -> product
  expect_equal(LCM(144, 233),  144 * 233)
})

test_that("LCM more than two values", {
  expect_equal(LCM(c(2, 3, 5, 7) * 11), 2 * 3 * 5 * 7 * 11)
  expect_equal(LCM(2*3, 3*5, 5*7),      2 * 3 * 5 * 7)
  expect_equal(LCM(4, 6, 10),           60)
})

test_that("LCM na.rm behaviour", {
  expect_true(is.na(LCM(4, NA, 6)))
  expect_equal(LCM(4, NA, 6, na.rm = TRUE), 12)
})

test_that("LCM errors on non-integer input", {
  expect_error(LCM(1.5, 2))
  expect_error(LCM(5))   # length < 2
})

