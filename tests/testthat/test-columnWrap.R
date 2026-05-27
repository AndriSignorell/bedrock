

library(testthat)


test_that("columnWrap returns matrix", {
  
  res <- columnWrap("This is a long text", width = 5)
  
  expect_true(is.matrix(res))
  
})



test_that("columnWrap wraps long strings", {
  
  res <- columnWrap("abcdefghij", width = 3)
  
  expect_true(is.matrix(res))
  
  expect_true(
    any(grepl("abc", res))
  )
  
})


test_that("columnWrap handles multiple columns", {
  
  res <- columnWrap(c("abcdef", "ghijkl"), width = 3)
  
  expect_equal(ncol(res), 2)
  
})


test_that("columnWrap recycles width", {
  
  res <- columnWrap(c("abcdef", "ghijkl"), width = 3)
  
  expect_true(is.matrix(res))
  
})
