library(testthat)

test_that("trim trims fraction", {

  x <- 1:10

  res <- trim(x, trim=0.1)

  expect_equal(length(res), 8)
})

test_that("trim trims fixed number", {

  x <- 1:10

  res <- trim(x, trim=2)

  expect_equal(length(res), 6)
})

test_that("trim handles NA removal", {

  x <- c(1:10, NA)

  res <- trim(x, trim=0.1, na.rm=TRUE)

  expect_false(any(is.na(res)))
})

test_that("trim returns NA for too much trimming", {

  expect_true(is.na(trim(1:10, trim=0.5)))
  expect_true(is.na(trim(1:10, trim=5)))
})

test_that("trim errors for invalid trim", {

  expect_error(
    trim(1:10, trim=c(0.1,0.2))
  )
})

test_that("trim errors for complex vectors", {

  expect_error(
    trim(1+1i, trim=0.1)
  )
})

test_that("trim stores trimmed indices", {

  x <- 1:10

  res <- trim(x, trim=0.1)

  expect_false(is.null(attr(res, "trim")))
})
