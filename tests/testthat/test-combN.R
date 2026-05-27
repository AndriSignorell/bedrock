

library(testthat)


test_that("combN computes combinations without replacement and without order", {
  
  expect_equal(
    combN(5, 2, repl = FALSE, ord = FALSE),
    choose(5, 2)
  )
  
})


test_that("combN computes permutations without replacement", {
  
  expect_equal(
    combN(5, 2, repl = FALSE, ord = TRUE),
    20
  )
  
})


test_that("combN computes combinations with replacement", {
  
  expect_equal(
    combN(5, 2, repl = TRUE, ord = FALSE),
    choose(6, 2)
  )
  
})


test_that("combN computes ordered combinations with replacement", {
  
  expect_equal(
    combN(5, 2, repl = TRUE, ord = TRUE),
    25
  )
  
})


test_that("combN works for m = 0", {
  
  expect_equal(
    combN(5, 0),
    1
  )
  
})
