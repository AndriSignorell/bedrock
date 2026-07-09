library(testthat)

test_that("dotProd computes vector dot product", {
  expect_equal(dotProd(c(1,2,3), c(4,5,6)), 32)
})

test_that("dotProd works for complex vectors", {
  res <- dotProd(c(1+1i, 2), c(3, 4-1i))
  expect_true(is.complex(res))
})

test_that("dotProd works for matrices", {
  x <- matrix(1:4, ncol=2)
  y <- matrix(4:1, ncol=2)

  expect_equal(
    dotProd(x, y),
    diag(crossprod(x, y))
  )
})

test_that("dotProd errors on unequal vector length", {
  expect_error(dotProd(1:3, 1:2))
})

test_that("dotProd errors on unequal matrix dimensions", {
  expect_error(
    dotProd(matrix(1:4,2), matrix(1:9,3))
  )
})
