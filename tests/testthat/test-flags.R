library(testthat)

test_that("flags extracts dichotomous variables", {

  dat <- data.frame(
    a=c(0,1,1),
    b=c(1,2,3),
    c=c(TRUE,FALSE,TRUE)
  )

  res <- flags(dat)

  expect_equal(colnames(res), c("a","c"))
})

test_that("flags returns names", {

  dat <- data.frame(
    a=c(0,1),
    b=c(1,2)
  )

  expect_equal(
    flags(dat, output = "names"),
    c("a", "b")
  )
})




test_that("flags returns indices", {

  dat <- data.frame(
    a=c(0,1),
    b=c(1,2)
  )

  expect_equal(
    flags(dat, output = "index"),
    c(a = 1, b = 2)
  )
  
})

test_that("flags errors on invalid input", {
  expect_error(flags(1:5))
})
