

library(testthat)


test_that("allIdentical returns TRUE for identical objects", {
  
  x <- 1:5
  
  expect_true(
    allIdentical(x, x, x)
  )
  
})


test_that("allIdentical returns FALSE for different objects", {
  
  expect_false(
    allIdentical(1:5, 1:4)
  )
  
})


test_that("allIdentical returns TRUE for single object", {
  
  expect_true(
    allIdentical(1:5)
  )
  
})


test_that("allIdentical returns TRUE for no input", {
  
  expect_true(
    allIdentical()
  )
  
})


test_that("allIdentical distinguishes object classes", {
  
  expect_false(
    allIdentical(1:3, factor(1:3))
  )
  
})
