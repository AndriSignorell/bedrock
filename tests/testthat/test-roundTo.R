library(testthat)

test_that("roundTo rounds correctly", {
  expect_equal(roundTo(10, 3), 9)
  expect_equal(roundTo(1.3, 0.2), 1.2)
})

test_that("roundTo works with floor", {
  expect_equal(roundTo(1.26, 0.1, floor), 1.2)
})

test_that("roundTo works with ceiling", {
  expect_equal(roundTo(1.21, 0.1, ceiling), 1.3)
})

test_that("roundTo errors for invalid FUN", {
  expect_error(roundTo(1, 1, "abc"))
})
