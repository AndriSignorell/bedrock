library(testthat)

test_that("digitSum computes digit sums", {
  expect_equal(digitSum(124L), 7L)
  expect_equal(digitSum(c(10L, 99L)), c(1L, 18L))
})

test_that("digitSum handles negative integers", {
  expect_equal(digitSum(-123L), 6L)
})

test_that("digitSum handles zero", {
  expect_equal(digitSum(0L), 0L)
})

test_that("digitSum propagates NA", {
  expect_equal(digitSum(c(1L, NA_integer_)), c(1L, NA_integer_))
})

test_that("digitSum errors on non-integers", {
  expect_error(digitSum(1.5))
  expect_error(digitSum(1))
})
