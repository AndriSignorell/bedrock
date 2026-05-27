library(testthat)

test_that("isEuclid identifies Euclidean distances", {

  d <- dist(matrix(rnorm(20), ncol=2))

  expect_true(isEuclid(d))
})

test_that("isEuclid returns attributes", {

  d <- dist(matrix(rnorm(20), ncol=2))
  res <- isEuclid(d)

  expect_true(!is.null(attr(res, "eigenvalues")))
  expect_true(!is.null(attr(res, "min_eigenvalue")))
})

test_that("isEuclid errors on invalid input", {

  expect_error(isEuclid(matrix(1:4,2)))
})
