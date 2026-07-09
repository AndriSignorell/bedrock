library(testthat)

test_that("splitAt splits at given positions", {
  res <- splitAt(1:10, c(4, 7))
  expect_equal(res, list(1:3, 4:6, 7:10))
})

test_that("unsorted and duplicate positions are handled", {
  expect_equal(splitAt(1:10, c(7, 4, 4, 20)),
               splitAt(1:10, c(4, 7)))
})

test_that("pos = 1 does not create an empty leading segment", {
  res <- splitAt(1:5, 1)
  expect_equal(res, list(1:5))
  expect_true(all(lengths(res) > 0L))
})

test_that("out-of-range positions are ignored", {
  expect_equal(splitAt(1:5, c(0, 3, 99)), list(1:2, 3:5))
})

test_that("no positions returns the whole vector", {
  expect_equal(splitAt(1:5, integer(0)), list(1:5))
})

test_that("empty input returns a single empty segment", {
  expect_equal(splitAt(integer(0), 3), list(integer(0)))
})

test_that("non-whole positions are rejected", {
  expect_error(splitAt(1:10, 4.5), "whole numbers")
  expect_error(splitAt(1:10, NA), "whole numbers")
})
