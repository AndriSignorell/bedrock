library(testthat)

test_that("getDotsArg extracts argument", {

  dots <- list(a=1, b=2)

  expect_equal(
    getDotsArg(dots, "a"),
    1
  )
})

test_that("getDotsArg returns default", {

  expect_equal(
    getDotsArg(list(), "x", default=99),
    99
  )
})

test_that("getDotsArg errors for invalid name", {

  expect_error(getDotsArg(list(), 1))
  expect_error(getDotsArg(list(), c("a","b")))
})
