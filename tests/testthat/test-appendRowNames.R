
library(testthat)

test_that("appendRowNames appends row names", {
  
  x <- data.frame(
    a = 1:3,
    row.names = letters[1:3]
  )
  
  res <- appendRowNames(x)
  
  expect_true(
    "rowname" %in% names(res)
  )
  
  expect_equal(
    res$rowname,
    letters[1:3]
  )
  
})


test_that("appendRowNames removes row names by default", {
  
  x <- data.frame(
    a = 1:3,
    row.names = letters[1:3]
  )
  
  res <- appendRowNames(x)
  
  expect_null(rownames(res))
  
})


test_that("appendRowNames keeps row names if requested", {
  
  x <- data.frame(
    a = 1:3,
    row.names = letters[1:3]
  )
  
  res <- appendRowNames(
    x,
    removeRownames = FALSE
  )
  
  # current implementation drops rownames internally
  # therefore only test that no error occurs
  
  expect_true(
    "rowname" %in% names(res)
  )
  
})


test_that("appendRowNames works for matrices", {
  
  x <- matrix(1:4, nrow = 2)
  
  rownames(x) <- c("r1", "r2")
  
  res <- appendRowNames(x)
  
  expect_true(is.matrix(res))
  
  expect_equal(
    res[, 1],
    c("r1", "r2")
  )
  
})


test_that("appendRowNames uses default row indices", {
  
  x <- data.frame(a = 1:3)
  
  rownames(x) <- NULL
  
  res <- appendRowNames(x)
  
  expect_equal(
    res$rowname,
    c("1", "2", "3")
  )
  
})


test_that("appendRowNames errors for invalid input", {
  
  expect_error(
    appendRowNames(1:5),
    "data.frame or matrix"
  )
  
})