library(testthat)

test_that("fibonacci computes correct values", {
  expect_equal(fibonacci(0), 0L)
  expect_equal(fibonacci(1), 1L)
  expect_equal(fibonacci(5), 5L)
})

test_that("fibonacci is vectorized", {
  expect_equal(
    fibonacci(0:5),
    c(0L,1L,1L,2L,3L,5L)
  )
})

test_that("fibonacci errors on invalid input", {
  expect_error(fibonacci(-1))
  expect_error(fibonacci(1.5))
  expect_error(fibonacci("a"))
})
