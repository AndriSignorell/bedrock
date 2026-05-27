library(testthat)

test_that("vShift shifts right", {

  expect_equal(
    vShift(1:5, 2),
    c(NA, NA, 1, 2, 3)
  )
})

test_that("vShift shifts left", {

  expect_equal(
    vShift(1:5, -2),
    c(3,4,5,NA,NA)
  )
})

test_that("vShift handles zero shift", {

  expect_equal(
    vShift(1:5, 0),
    1:5
  )
})

test_that("vShift handles large shifts", {

  expect_equal(
    vShift(1:5, 10),
    rep(NA_integer_, 5)
  )
  
})

test_that("vShift warns for non-integer k", {

  expect_warning(
    vShift(1:5, 1.2)
  )
})
