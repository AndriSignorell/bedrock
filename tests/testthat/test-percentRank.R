library(testthat)

test_that("percentRank computes ranks", {

  x <- c(10,20,20,30)

  expect_equal(
    percentRank(x),
    c(0, 1/3, 1/3, 1)
  )
})

test_that("percentRank preserves NA", {

  x <- c(1, NA, 2)

  res <- percentRank(x)

  expect_true(is.na(res[2]))
})

test_that("percentRank returns NA for short vectors", {

  expect_true(all(is.na(percentRank(c(5, NA)))))
})
