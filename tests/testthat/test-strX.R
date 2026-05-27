
# =========================================================
# test-strX.R
# =========================================================

library(testthat)

test_that("strX returns invisible character vector", {
  
  res <- strX(mtcars)
  
  expect_type(res, "character")
  
})

test_that("strX numbers variables", {
  
  out <- capture.output(
    strX(mtcars)
  )
  
  expect_true(
    any(grepl("1 \\$", out))
  )
  
})

test_that("strX works without numbering", {
  
  out <- capture.output(
    strX(
      mtcars,
      number.variables = FALSE
    )
  )
  
  expect_false(
    any(grepl("1 \\$", out))
  )
  
})

test_that("strX supports recursive numbering", {
  
  x <- list(
    a = 1,
    b = list(
      c = 2
    )
  )
  
  out <- capture.output(
    strX(
      x,
      recursive = TRUE
    )
  )
  
  expect_true(
    any(grepl("1 \\$", out))
  )
  
})

test_that("strX errors on invalid logical arguments", {
  
  expect_error(
    strX(mtcars, recursive = 1:2)
  )
  
  expect_error(
    strX(mtcars, number.variables = NA)
  )
  
})

