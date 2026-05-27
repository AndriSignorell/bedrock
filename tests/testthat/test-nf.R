library(testthat)

test_that("nf converts factor codes", {

  expect_equal(
    nf(c("a","b","a")),
    c(1,2,1)
  )
})

test_that("nf respects factor levels", {

  expect_equal(
    nf(c("low","high"), levels=c("low","high")),
    c(1,2)
  )
})
