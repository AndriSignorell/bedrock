library(testthat)

test_that("extractArgs merges defaults and dots", {
  dots <- list(a=10)
  defaults <- list(a=1, b=2)

  expect_equal(
    extractArgs(dots, defaults),
    list(a=10, b=2)
  )
})

test_that("extractArgs ignores unknown arguments", {
  dots <- list(c=3)
  defaults <- list(a=1)

  expect_equal(
    extractArgs(dots, defaults),
    list(a=1)
  )
})

test_that("extractArgs returns rest arguments", {
  dots <- list(a=1, z=9)
  defaults <- list(a=0)

  res <- extractArgs(dots, defaults, returnRest=TRUE)

  expect_equal(res$args, list(a=1))
  expect_equal(res$rest, list(z=9))
})

test_that("extractArgs calls validation function", {
  expect_error(
    extractArgs(
      list(a=-1),
      list(a=1),
      validate=function(x){
        if(x$a < 0) stop("invalid")
      }
    )
  )
})
