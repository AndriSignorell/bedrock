library(testthat)

test_that("randGroupSplit splits correctly", {

  set.seed(1)

  res <- randGroupSplit(1:10, c(3,3,4))

  expect_equal(
    unname(lengths(res)),
    c(3,3,4)
  )
})

test_that("randGroupSplit errors for invalid sizes", {

  expect_error(
    randGroupSplit(1:10, c(3,-1))
  )
})

test_that("randGroupSplit errors for unequal total", {

  expect_error(
    randGroupSplit(1:10, c(3,3))
  )
})
