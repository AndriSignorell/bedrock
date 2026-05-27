library(testthat)

test_that("nunique counts unique values", {

  expect_equal(
    nunique(c(1,1,2,3)),
    3
  )
})

test_that("nunique handles NA", {

  expect_equal(
    nunique(c(1,1,NA)),
    2
  )
})

test_that("nunique removes NA", {

  expect_equal(
    nunique(c(1,1,NA), na.rm=TRUE),
    1
  )
})
