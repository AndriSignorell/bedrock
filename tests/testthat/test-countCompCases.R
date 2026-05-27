
library(testthat)


test_that("countCompCases returns correct structure", {
  
  x <- data.frame(
    a = c(1, NA, 3),
    b = c(1, 2, NA)
  )
  
  res <- countCompCases(x)
  
  expect_true(is.list(res))
  expect_true("tab" %in% names(res))
  
})


test_that("countCompCases counts complete cases correctly", {
  
  x <- data.frame(
    a = c(1, NA, 3),
    b = c(1, 2, NA)
  )
  
  res <- countCompCases(x)
  
  expect_equal(res$n, 3)
  expect_equal(res$cc, 1)
  
})


test_that("countCompCases returns expected columns", {
  
  x <- data.frame(
    a = c(1, NA),
    b = c(NA, 2)
  )
  
  res <- countCompCases(x)
  
  expect_true(all(c(
    "vname",
    "nas",
    "nas_p",
    "cifnot",
    "cifnot_p"
  ) %in% names(res$tab)))
  
})


test_that("countCompCases sets correct class", {
  
  x <- data.frame(a = c(1, NA))
  
  res <- countCompCases(x)
  
  expect_s3_class(res, "CountCompCases")
  
})
