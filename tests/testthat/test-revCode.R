library(testthat)

test_that("numeric reverse works with implicit range", {
  x <- c(1, 2, 3, 4, 5)
  expect_equal(revCode(x), c(5, 4, 3, 2, 1))
})

test_that("numeric reverse works with explicit min/max", {
  x <- c(1, 2, 3)
  expect_equal(revCode(x, min = 1, max = 5), c(5, 4, 3))
})

test_that("numeric handles NA correctly with na.rm = TRUE", {
  x <- c(1, 2, NA, 4)
  res <- revCode(x, na.rm = TRUE)
  expect_equal(res, c(4, 3, NA, 1))
})

test_that("numeric fails if all values are NA", {
  x <- c(NA, NA)
  expect_error(revCode(x, na.rm = TRUE))
})

test_that("numeric min/max validation works", {
  expect_error(revCode(1:3, min = "a", max = 5))
  expect_error(revCode(1:3, min = 1, max = "b"))
})

test_that("logical reverse works", {
  x <- c(TRUE, FALSE, TRUE)
  expect_equal(revCode(x), c(FALSE, TRUE, FALSE))
})

test_that("logical preserves NA", {
  x <- c(TRUE, NA, FALSE)
  expect_equal(revCode(x), c(FALSE, NA, TRUE))
})

test_that("factor levels are reversed correctly", {
  x <- factor(c("low", "medium", "high"), 
              levels=c("low", "medium", "high"), ordered = TRUE)
  res <- revCode(x)
  
  expect_equal(levels(res), c("high", "medium", "low"))
  expect_true(is.ordered(res))
})


test_that("factor values are correctly remapped", {
  x <- factor(c("low", "medium", "high"), 
              levels=c("low", "medium", "high"), ordered = TRUE)
  res <- revCode(x)
  
  expect_equal(as.character(res), c("high", "medium", "low"))
})

test_that("factor with unused levels works", {
  x <- factor(c("low", "low"), levels = c("low", "medium", "high"))
  res <- revCode(x)
  
  expect_equal(levels(res), c("high", "medium", "low"))
  expect_equal(as.character(res), c("high", "high"))
})

test_that("factor with single level is unchanged", {
  x <- factor(c("only", "only"))
  res <- revCode(x)
  
  expect_equal(res, x)
})

test_that("unsupported types throw error", {
  expect_error(revCode("a"))
  expect_error(revCode(list(1, 2, 3)))
})

test_that("output type is preserved", {
  expect_true(is.numeric(revCode(1:3)))
  expect_true(is.logical(revCode(c(TRUE, FALSE))))
  expect_true(is.factor(revCode(factor(c("a", "b")))))
})

test_that("length is preserved", {
  x <- sample(1:10, 100, replace = TRUE)
  expect_equal(length(revCode(x)), length(x))
})

