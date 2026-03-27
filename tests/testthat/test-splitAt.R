
library(testthat)

test_that("splitAt splits at correct positions", {
  x <- 1:10
  res <- splitAt(x, c(4, 7))
  
  expect_equal(res, list(1:3, 4:6, 7:10))
})

test_that("splitAt works with unsorted positions", {
  x <- 1:10
  res <- splitAt(x, c(7, 4))
  
  expect_equal(res, list(1:3, 4:6, 7:10))
})

test_that("splitAt removes duplicate positions", {
  x <- 1:10
  res <- splitAt(x, c(4, 4, 7))
  
  expect_equal(res, list(1:3, 4:6, 7:10))
})

test_that("splitAt ignores out-of-bound positions", {
  x <- 1:10
  res <- splitAt(x, c(0, 4, 11))
  
  expect_equal(res, list(1:3, 4:10))
})

test_that("splitAt works with no positions", {
  x <- 1:5
  res <- splitAt(x, integer(0))
  
  expect_equal(res, list(1:5))
})

test_that("splitAt works with single split", {
  x <- 1:5
  res <- splitAt(x, 3)
  
  expect_equal(res, list(1:2, 3:5))
})

test_that("splitAt works with character vectors", {
  x <- letters[1:6]
  res <- splitAt(x, c(3, 5))
  
  expect_equal(res, list(c("a", "b"), c("c", "d"), c("e", "f")))
})

test_that("splitAt returns list of correct lengths", {
  x <- 1:10
  res <- splitAt(x, c(4, 7))
  
  expect_equal(length(res), 3)
  expect_equal(lengths(res), c(3, 3, 4))
})

test_that("splitAt handles split at first position", {
  x <- 1:5
  res <- splitAt(x, 1)
  
  expect_equal(res, list(integer(0), 1:5))
})

test_that("splitAt handles split at last position", {
  x <- 1:5
  res <- splitAt(x, 5)
  
  expect_equal(res, list(1:4, 5))
})