
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


test_that("zero is neutral for the GCD and absorbing for the LCM", {
  
  # REGRESSION: LCM() carried GCD()'s line `x <- abs(x[x != 0])`. Dropping
  # the zeros is right for the greatest common divisor - every number
  # divides 0 - but wrong for the least common multiple, where 0 is a
  # multiple of every number and therefore the smallest one. LCM(0, 6)
  # returned 6.
  expect_equal(LCM(0, 6), 0)
  expect_equal(LCM(6, 0), 0)
  expect_equal(LCM(0, 0), 0)
  expect_equal(LCM(0, 3, 5), 0)
  
  expect_equal(GCD(0, 6), 6)
  expect_equal(GCD(6, 0), 6)
  expect_equal(GCD(0, 0), 0)
  expect_equal(GCD(0, 3, 5), 1)
})


test_that("n * m == GCD(n, m) * LCM(n, m), zero included", {
  
  # the relation the help page promises - and the shortest way to see that
  # LCM(0, 6) has to be 0
  pairs <- list(c(0, 6), c(6, 0), c(0, 0), c(12, 10), c(144, 233),
                c(-4, 6), c(7, 7), c(1, 13))
  
  for (p in pairs)
    expect_equal(GCD(p[1], p[2]) * LCM(p[1], p[2]), abs(p[1] * p[2]),
                 info = paste(p, collapse = ", "))
})


test_that("the documented values are unchanged", {
  
  expect_equal(GCD(12, 10), 2)
  expect_equal(GCD(144, 233), 1)      # consecutive Fibonacci numbers
  expect_equal(LCM(12, 10), 60)
  expect_equal(LCM(144, 233), 144 * 233)
  
  expect_equal(GCD(2, 3, c(5, 7) * 11), 1)
  expect_equal(GCD(c(2*3, 3*5, 5*7)), 1)
  expect_equal(LCM(c(2, 3, 5, 7) * 11), 2*3*5*7*11)
  expect_equal(LCM(2*3, 3*5, 5*7), 2*3*5*7)
})


test_that("negative values enter through their absolute value", {
  
  expect_equal(GCD(-4, 6), 2)
  expect_equal(GCD(4, -6), 2)
  expect_equal(GCD(-4, -6), 2)
  
  expect_equal(LCM(-4, 6), 12)
  expect_equal(LCM(4, -6), 12)
  expect_equal(LCM(-4, -6), 12)
})


test_that("logical vectors are coerced, and NA follows na.rm", {
  
  expect_equal(GCD(c(TRUE, TRUE), 4L), 1)
  expect_equal(LCM(c(TRUE, FALSE), 4L), 0)   # FALSE is 0, hence absorbing
  
  expect_true(is.na(GCD(12, NA)))
  expect_true(is.na(LCM(12, NA)))
  
  expect_equal(GCD(12, 10, NA, na.rm = TRUE), 2)
  expect_equal(LCM(12, 10, NA, na.rm = TRUE), 60)
})


test_that("non-finite and non-whole values are refused", {
  
  # floor(Inf) == ceiling(Inf), so an infinite value passed the whole-number
  # test and was then converted to long long, which is undefined behaviour
  expect_error(GCD(Inf, 6), "finite")
  expect_error(LCM(6, -Inf), "finite")
  
  expect_error(GCD(1.5, 3), "whole numbers")
  expect_error(LCM(1.5, 3), "whole numbers")
  
  expect_error(GCD(12), "at least 2")
  expect_error(LCM(12), "at least 2")
  
  expect_error(GCD("a", "b"), "integer or logical")
  expect_error(LCM("a", "b"), "integer or logical")
})

