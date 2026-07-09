library(testthat)

test_that("funCalls returns list", {

  res <- funCalls("mean")

  expect_true(is.list(res))
})

test_that("funCalls supports sorting", {

  res <- funCalls("mean", sorted=TRUE)

  expect_true(is.list(res))
})
