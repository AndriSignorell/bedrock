library(testthat)

test_that("isOdd detects odd numbers", {

  expect_equal(
    isOdd(1:5),
    c(TRUE, FALSE, TRUE, FALSE, TRUE)
  )
})

test_that("isOdd returns NA for non-integers", {

  expect_true(is.na(isOdd(1.5)))
})

test_that("isOdd returns NA for non-finite values", {

  expect_true(is.na(isOdd(Inf)))
  expect_true(is.na(isOdd(NA)))
})
