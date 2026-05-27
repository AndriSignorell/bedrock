library(testthat)

test_that("rankX computes ranks", {

  expect_equal(
    rankX(c(10,20,20,30)),
    c(1,2.5,2.5,4)
  )
})

test_that("rankX supports dense ranking", {

  expect_equal(
    rankX(c(1,2,2,3), ties.method="dense"),
    c(1,2,2,3)
  )
})

test_that("rankX supports decreasing order", {

  expect_equal(
    rankX(c(1,2,3), decreasing=TRUE),
    c(3,2,1)
  )
})

test_that("rankX errors for unequal lengths", {

  expect_error(
    rankX(1:3, 1:2)
  )
})
