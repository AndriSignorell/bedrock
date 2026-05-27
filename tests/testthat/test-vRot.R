library(testthat)

test_that("vRot rotates right", {

  expect_equal(
    vRot(1:5, 2),
    c(4,5,1,2,3)
  )
})

test_that("vRot rotates left with negative k", {

  expect_equal(
    vRot(1:5, -1),
    c(2,3,4,5,1)
  )
})

test_that("vRot handles k = 0", {

  expect_equal(
    vRot(1:5, 0),
    1:5
  )
})

test_that("vRot handles empty vectors", {

  expect_equal(
    vRot(integer(0)),
    integer(0)
  )
})

test_that("vRot warns for non-integer k", {

  expect_warning(
    vRot(1:5, 1.7)
  )
})
