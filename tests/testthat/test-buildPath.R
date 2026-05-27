

library(testthat)


test_that("buildPath constructs paths correctly", {
  
  p <- buildPath("data", "file.csv")
  
  expect_true(grepl("file\\.csv$", p))
  
})


test_that("buildPath works with trailing slash", {
  
  p1 <- buildPath("data", "file.csv")
  p2 <- buildPath("data/", "file.csv")
  
  expect_equal(basename(p1), basename(p2))
  
})


test_that("buildPath returns character scalar", {
  
  res <- buildPath("abc", "def")
  
  expect_type(res, "character")
  expect_length(res, 1)
  
})
