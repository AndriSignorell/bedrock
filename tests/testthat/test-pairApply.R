library(testthat)

test_that("pairApply computes pairwise matrix", {

  x <- data.frame(
    a=1:5,
    b=6:10
  )

  res <- pairApply(x, cor)

  expect_true(is.matrix(res))
  expect_equal(dim(res), c(2,2))
})

test_that("pairApply works symmetrically", {

  x <- data.frame(
    a=1:5,
    b=6:10
  )

  res <- pairApply(x, cor, symmetric=TRUE)

  expect_equal(res[1,2], res[2,1])
})
