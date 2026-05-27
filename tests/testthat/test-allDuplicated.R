

library(testthat)


test_that("allDuplicated identifies all tied values", {
  
  x <- c(1, 2, 2, 3, 4, 4)
  
  expect_equal(
    allDuplicated(x),
    c(FALSE, TRUE, TRUE, FALSE, TRUE, TRUE)
  )
  
})


test_that("allDuplicated returns all FALSE for unique values", {
  
  x <- 1:5
  
  expect_equal(
    allDuplicated(x),
    rep(FALSE, 5)
  )
  
})


test_that("allDuplicated handles NA values", {
  
  x <- c(1, NA, 2, NA)
  
  expect_equal(
    allDuplicated(x),
    c(FALSE, TRUE, FALSE, TRUE)
  )
  
})


test_that("allDuplicated works with characters", {
  
  x <- c("a", "b", "a", "c")
  
  expect_equal(
    allDuplicated(x),
    c(TRUE, FALSE, TRUE, FALSE)
  )
  
})
