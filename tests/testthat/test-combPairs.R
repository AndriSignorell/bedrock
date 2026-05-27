

library(testthat)


test_that("combPairs returns all pairwise combinations", {
  
  res <- combPairs(letters[1:4])
  
  expect_equal(nrow(res), 6)
  expect_equal(names(res), c("X1", "X2"))
  
})


test_that("combPairs works with x and y", {
  
  res <- combPairs(1:2, letters[1:3])
  
  expect_equal(nrow(res), 6)
  
})


test_that("combPairs preserves duplicate values", {
  
  res <- combPairs(c(1, 1, 2))
  
  expect_true(any(duplicated(res)))
  
})
