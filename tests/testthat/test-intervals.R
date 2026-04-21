
library(testthat)

# ------------------------------------------------------------------------------
# overlap
# ------------------------------------------------------------------------------

test_that("overlap works for basic cases", {
  expect_equal(overlap(c(1, 5), c(3, 7)), 2)
  expect_equal(overlap(c(1, 2), c(3, 4)), 0)
  expect_equal(overlap(c(1, 5), c(1, 5)), 4)
})

test_that("overlap handles reversed intervals", {
  expect_equal(overlap(c(5, 1), c(7, 3)), 2)
})

test_that("overlap handles touching intervals", {
  expect_equal(overlap(c(1, 3), c(3, 5)), 0)
})

test_that("overlap is vectorized", {
  x <- matrix(c(1,5,
                1,2), ncol = 2, byrow = TRUE)
  y <- matrix(c(3,7,
                3,4), ncol = 2, byrow = TRUE)
  
  expect_equal(overlap(x, y), c(2, 0))
})

test_that("overlap recycles correctly", {
  x <- matrix(c(1,5,
                2,6), ncol = 2, byrow = TRUE)
  y <- c(3,7)
  
  expect_equal(overlap(x, y), c(2, 3))
})

# ------------------------------------------------------------------------------
# overlaps (logical)
# ------------------------------------------------------------------------------

test_that("overlaps works correctly", {
  expect_true(overlaps(c(1,5), c(3,7)))
  expect_false(overlaps(c(1,2), c(3,4)))
})

test_that("overlaps matches overlap > 0", {
  x <- matrix(c(1,5,
                1,2), ncol = 2, byrow = TRUE)
  y <- matrix(c(3,7,
                3,4), ncol = 2, byrow = TRUE)
  
  expect_equal(overlaps(x, y), overlap(x, y) > 0)
})

# ------------------------------------------------------------------------------
# distance
# ------------------------------------------------------------------------------

test_that("distance works for non-overlapping intervals", {
  expect_equal(distance(c(1,2), c(4,5)), 2)
  expect_equal(distance(c(4,5), c(1,2)), 2)
})

test_that("distance is zero for overlapping intervals", {
  expect_equal(distance(c(1,5), c(3,7)), 0)
})

test_that("distance handles touching intervals", {
  expect_equal(distance(c(1,3), c(3,5)), 0)
})

test_that("distance is vectorized", {
  x <- matrix(c(1,2,
                1,5), ncol = 2, byrow = TRUE)
  y <- matrix(c(4,5,
                3,7), ncol = 2, byrow = TRUE)
  
  expect_equal(distance(x, y), c(2, 0))
})

# ------------------------------------------------------------------------------
# operator
# ------------------------------------------------------------------------------

test_that("%overlaps% works as expected", {
  expect_true(c(1,5) %overlaps% c(3,7))
  expect_false(c(1,2) %overlaps% c(3,4))
})

# ------------------------------------------------------------------------------
# edge cases
# ------------------------------------------------------------------------------

test_that("zero-length intervals behave correctly", {
  expect_equal(overlap(c(3,3), c(3,3)), 0)
  expect_true(overlaps(c(3,3), c(3,3)))
  expect_equal(distance(c(3,3), c(3,3)), 0)
})

test_that("NA handling propagates", {
  expect_true(is.na(overlap(c(NA, 5), c(3,7))))
  expect_true(is.na(distance(c(1,2), c(NA, 5))))
})

test_that("invalid input throws error", {
  expect_error(overlap(matrix(1:3, ncol=3), c(1,2)))
})