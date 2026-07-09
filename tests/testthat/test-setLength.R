library(testthat)

test_that("setLength pads with fill", {
  expect_equal(setLength(LETTERS[1:3], 5), c("A", "B", "C", NA, NA))
  expect_equal(setLength(1:4, 6, fill = 0), c(1:4, 0, 0))
})

test_that("setLength truncates", {
  expect_equal(setLength(LETTERS[1:3], 2), c("A", "B"))
})

test_that("n equal to length returns x unchanged", {
  expect_equal(setLength(1:3, 3), 1:3)
})

test_that("n = 0 returns empty vector", {
  expect_length(setLength(1:3, 0), 0L)
})

test_that("invalid n is rejected", {
  expect_error(setLength(1:3, -1), "non-negative")
  expect_error(setLength(1:3, 2.5), "whole number")
  expect_error(setLength(1:3, c(1, 2)), "single")
  expect_error(setLength(1:3, NA), "single non-negative")
})

test_that("names are preserved when padding", {
  x <- c(a = 1, b = 2)
  res <- setLength(x, 4, fill = 0)
  expect_equal(names(res)[1:2], c("a", "b"))
})
