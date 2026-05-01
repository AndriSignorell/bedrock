
library(testthat)

# ---------------------------

# skip behavior

# ---------------------------

test_that("callIf skips when arg is FALSE/NULL/NA", {
  f <- function() 1
  
  expect_null(callIf(f, FALSE))
  expect_null(callIf(f, NULL))
  expect_null(callIf(f, NA))
})

# ---------------------------

# TRUE uses defaults

# ---------------------------

test_that("callIf with TRUE uses defaults", {
  f <- function(x) x + 1
  
  expect_equal(
    callIf(f, TRUE, defaults = list(x = 1)),
    2
  )
})

# ---------------------------

# TRUE without defaults

# ---------------------------

test_that("callIf with TRUE calls function without args", {
  f <- function() 42
  
  expect_equal(callIf(f, TRUE), 42)
})

# ---------------------------

# list passes arguments

# ---------------------------

test_that("callIf passes named list as arguments", {
  f <- function(x, y) x + y
  
  expect_equal(
    callIf(f, list(x = 2, y = 3)),
    5
  )
})

# ---------------------------

# defaults + override

# ---------------------------

test_that("callIf merges defaults and arg correctly", {
  f <- function(x, y) x + y
  
  expect_equal(
    callIf(f, list(y = 5), defaults = list(x = 2, y = 1)),
    7
  )
})

# ---------------------------

# forbidden arguments removed

# ---------------------------

test_that("callIf removes forbidden arguments", {
  f <- function(x, y = 0) x + y
  
  expect_warning(
    res <- callIf(
      f,
      list(x = 2, y = 3),
      forbidden = "y"
    )
  )
  
  expect_equal(res, 2)
})

# ---------------------------

# forbidden without warning

# ---------------------------

test_that("callIf suppresses warning when warn = FALSE", {
  f <- function(x) x
  
  expect_silent(
    callIf(
      f,
      list(x = 1, y = 2),
      forbidden = "y",
      warn = FALSE
    )
  )
})

# ---------------------------

# invalid arg type

# ---------------------------

test_that("callIf errors on invalid arg", {
  f <- function(x) x
  
  expect_error(callIf(f, 123))
  expect_error(callIf(f, "abc"))
})

# ---------------------------

# unnamed list rejected

# ---------------------------

test_that("callIf requires named list", {
  f <- function(x) x
  
  expect_error(callIf(f, list(1, 2)))
})

# ---------------------------

# return value propagated

# ---------------------------

test_that("callIf returns result of function", {
  f <- function(x) x * 2
  
  expect_equal(callIf(f, list(x = 3)), 6)
})

# ---------------------------

# side-effect usage

# ---------------------------

test_that("callIf works with side-effect functions", {
  
  expect_silent(callIf(message, FALSE))
  
  expect_message(
    callIf(message, list(x = "hello")),
    "hello"
  )
})





