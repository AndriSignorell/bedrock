

library(testthat)


test_that("combLevels combines levels from factors", {
  
  x <- factor(c("A", "B"))
  y <- factor(c("B", "C"))
  
  expect_equal(
    combLevels(x, y),
    c("A", "B", "C")
  )
  
})


test_that("combLevels coerces character vectors", {
  
  expect_equal(
    combLevels(c("A", "B"), c("C")),
    c("A", "B", "C")
  )
  
})


test_that("combLevels sorts levels", {
  
  expect_equal(
    combLevels(c("B", "A"), sort = TRUE),
    c("A", "B")
  )
  
})


test_that("combLevels handles empty input", {
  
  expect_equal(
    combLevels(),
    character()
  )
  
})


test_that("combLevels includes NA when requested", {
  
  res <- combLevels(c("A", NA), na = TRUE)
  
  expect_true(any(is.na(res)))
  
})
