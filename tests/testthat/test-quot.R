library(testthat)

test_that("quot computes quotients", {

  expect_equal(
    quot(c(1,2,4,8)),
    c(2,2,2)
  )
})

test_that("quot works for matrices", {

  m <- matrix(c(1,2,4,8), ncol=1)

  res <- quot(m)

  expect_true(is.matrix(res))
})

test_that("quot handles higher order quotients", {

  expect_equal(
    quot(c(1,2,4,8), quotients=2),
    c(1,1)
  )
})

test_that("quot errors for invalid lag", {

  expect_error(
    quot(1:10, lag=0)
  )
})
