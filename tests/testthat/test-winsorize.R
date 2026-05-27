library(testthat)

test_that("winsorize limits extreme values", {

  x <- c(-100, 1:10, 100)

  res <- winsorize(x, val=c(0,10))

  expect_equal(min(res), 0)
  expect_equal(max(res), 10)
})

test_that("winsorize preserves NA", {

  x <- c(1,2,NA,100)

  res <- winsorize(x, val=c(0,10))

  expect_true(is.na(res[3]))
})

test_that("winsorize returns same length", {

  x <- rnorm(20)

  expect_length(
    winsorize(x),
    length(x)
  )
})
