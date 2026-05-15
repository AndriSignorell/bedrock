

library(testthat)

test_that("linScale rescales to [0,1] by default", {
  x <- matrix(1:10, ncol = 2)
  res <- linScale(x)
  
  expect_equal(apply(res, 2, min), c(0, 0))
  expect_equal(apply(res, 2, max), c(1, 1))
})

test_that("linScale works with custom range", {
  x <- matrix(1:10, ncol = 2)
  res <- linScale(x, newLow = -1, newHigh = 1)
  
  expect_equal(apply(res, 2, min), c(-1, -1))
  expect_equal(apply(res, 2, max), c(1, 1))
})

test_that("linScale handles vector input", {
  x <- 1:5
  res <- linScale(x)
  
  expect_equal(as.numeric(res), seq(0, 1, length.out = 5))
})

test_that("LinScale respects provided low/high", {
  x <- matrix(1:10, ncol = 2)
  res <- linScale(x, low = 1, high = 10)
  
  expected <- (x[,1] - 1) / 9
  expect_equal(res[,1], expected)
})


test_that("linScale handles constant columns", {
  x <- cbind(1:5, rep(3, 5))
  res <- linScale(x)
  
  expect_equal(res[,2], rep(0, 5))
})

test_that("linScale handles NA values", {
  x <- matrix(c(1,2,NA,4,5,6), ncol = 2)
  res <- linScale(x)
  
  expect_true(any(is.na(res)))
})

test_that("linScale returns correct dimensions", {
  x <- matrix(1:12, ncol = 3)
  res <- linScale(x)
  
  expect_equal(dim(res), dim(x))
})

test_that("linScale recycles newLow/newHigh", {
  x <- matrix(1:10, ncol = 2)
  res <- linScale(x, newLow = c(0, -1), newHigh = c(1, 1))
  
  expect_equal(apply(res, 2, min), c(0, -1))
  expect_equal(apply(res, 2, max), c(1, 1))
})
