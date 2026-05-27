
# =========================================================
# test-strSplitToDummy.R
# =========================================================

library(testthat)

test_that("strSplitToDummy creates dummy matrix", {
  
  x <- c("A,B", "B,C")
  
  res <- strSplitToDummy(x)
  
  expect_true(is.data.frame(res))
  expect_equal(colnames(res), c("A", "B", "C"))
  
})

test_that("strSplitToDummy trims whitespace", {
  
  x <- c("A, B")
  
  res <- strSplitToDummy(x)
  
  expect_equal(res$B, 1L)
  
})

test_that("strSplitToDummy handles NA with na.pass", {
  
  x <- c("A,B", NA)
  
  res <- strSplitToDummy(x)
  
  expect_equal(nrow(res), 2)
  
})

test_that("strSplitToDummy handles na.omit", {
  
  x <- c("A,B", NA)
  
  res <- strSplitToDummy(x, na.action = na.omit)
  
  expect_equal(nrow(res), 1)
  
})

test_that("strSplitToDummy handles na.exclude", {
  
  x <- c("A,B", NA)
  
  res <- strSplitToDummy(x, na.action = na.exclude)
  
  expect_true(!is.null(attr(res, "na.action")))
  
})

test_that("strSplitToDummy errors with na.fail", {
  
  expect_error(
    strSplitToDummy(c("A", NA), na.action = na.fail)
  )
  
})

test_that("strSplitToDummy warns for invalid names", {
  
  expect_warning(
    strSplitToDummy(c("A+B"))
  )
  
})

test_that("strSplitToDummy handles empty input", {
  
  res <- strSplitToDummy(character(0))
  
  expect_equal(nrow(res), 0)
  
})


