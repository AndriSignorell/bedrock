library(testthat)

test_that("isDichotomous detects binary vectors", {

  expect_true(
    isDichotomous(c(0,1,1))
  )
})

test_that("isDichotomous handles strict mode", {

  expect_false(
    isDichotomous(c(1,1,1), strict=TRUE)
  )
})

test_that("isDichotomous handles NA", {

  expect_true(
    is.na(isDichotomous(c(0,1,NA)))
  )

  expect_true(
    isDichotomous(c(0,1,NA), na.rm=TRUE)
  )
})

test_that("isDichotomous handles empty vectors", {

  expect_true(isDichotomous(numeric(0)))
  expect_false(isDichotomous(numeric(0), strict=TRUE))
})
