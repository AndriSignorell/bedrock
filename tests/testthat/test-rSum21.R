library(testthat)

test_that("rSum21 sums to one", {
  x <- rSum21(10)
  expect_equal(sum(x), 1)
})

test_that("rSum21 returns correct length", {
  expect_length(rSum21(5), 5)
})

test_that("rSum21 handles rounding", {
  x <- rSum21(10, digits = 2)
  expect_equal(round(sum(x), 10), 1)
})
