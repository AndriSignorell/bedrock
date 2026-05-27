library(testthat)

test_that("naIf replaces values with NA", {

  x <- c(1,2,3,2)

  expect_equal(
    naIf(x, 2),
    c(1, NA, 3, NA)
  )
})

test_that("naIf handles characters", {

  x <- c("a","b","a")

  expect_equal(
    naIf(x, "a"),
    c(NA, "b", NA)
  )
})
