library(testthat)

test_that("funCalls returns list", {

  res <- funCalls("mean")

  expect_true(is.list(res))
})

test_that("funCalls supports sorting", {

  res <- funCalls("mean", sort=TRUE)

  expect_true(is.list(res))
})
