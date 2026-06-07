
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
  
  res <- funArgs(mean, sort=TRUE)
  
  expect_true(is.data.frame(res))
})

test_that("funArgs returns empty FunArgs for primitives", {
  res <- funArgs(sum)
  expect_true(inherits(res, "FunArgs"))
  expect_equal(nrow(res), 0L)
})

