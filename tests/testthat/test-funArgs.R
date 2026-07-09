
library(testthat)

test_that("funArgs returns argument table", {
  
  res <- funArgs("mean")
  
  expect_true(is.data.frame(res))
  expect_true(all(c("name","value") %in% names(res)))
})

test_that("funArgs works with function objects", {
  
  res <- funArgs(mean)
  
  expect_true(is.data.frame(res))
})

test_that("funArgs sorts arguments", {
  
  res <- funArgs(mean, sorted=TRUE)
  
  expect_true(is.data.frame(res))
})

test_that("funArgs resolves primitives via args() stub", {
  res <- funArgs(sum)
  expect_true(inherits(res, "FunArgs"))
  # sum() has no formals, but args(sum) yields "function (..., na.rm = FALSE)"
  expect_gt(nrow(res), 0L)
  expect_true("na.rm" %in% res$name)
})

