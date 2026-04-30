
library(testthat)

# -------------------------------------------------------------------------
# basic functionality (data.frame)
# -------------------------------------------------------------------------

test_that("completeColumns identifies complete columns in data.frame", {
  
  df <- data.frame(
    a = c(1, 2, 3),
    b = c(1, NA, 3),
    c = c("x", "y", "z")
  )
  
  expect_equal(completeColumns(df), c("a", "c"))
})

test_that("completeColumns returns logical vector when which = FALSE", {
  
  df <- data.frame(
    a = c(1, 2, 3),
    b = c(1, NA, 3)
  )
  
  res <- completeColumns(df, which = FALSE)
  
  expect_type(res, "logical")
  expect_equal(unname(res), c(TRUE, FALSE))
  expect_equal(names(res), c("a", "b"))
})

# -------------------------------------------------------------------------
# list input
# -------------------------------------------------------------------------

test_that("completeColumns works on lists", {
  
  x <- list(
    a = 1:3,
    b = c(1, NA),
    c = letters[1:3]
  )
  
  expect_equal(completeColumns(x), c("a", "c"))
})

# -------------------------------------------------------------------------
# unnamed input
# -------------------------------------------------------------------------

test_that("completeColumns handles unnamed inputs", {
  
  x <- list(
    1:3,
    c(1, NA),
    letters[1:3]
  )
  
  expect_equal(completeColumns(x), c("1", "3"))
})

# -------------------------------------------------------------------------
# edge cases
# -------------------------------------------------------------------------

test_that("completeColumns works with empty input", {
  
  x <- list()
  
  expect_equal(completeColumns(x), character(0))
  expect_equal(completeColumns(x, which = FALSE), logical(0))
})

test_that("completeColumns handles all missing columns", {
  
  x <- list(
    a = c(NA, NA),
    b = c(NA, NA)
  )
  
  res <- completeColumns(x, which = FALSE)
  
  expect_equal(unname(res), c(FALSE, FALSE))
  expect_equal(names(res), c("a", "b"))
})

test_that("completeColumns handles no missing values", {
  
  x <- list(
    a = 1:3,
    b = 4:6
  )
  
  res <- completeColumns(x, which = FALSE)
  
  expect_equal(unname(res), c(TRUE, TRUE))
  expect_equal(names(res), c("a", "b"))
})

# -------------------------------------------------------------------------
# special values
# -------------------------------------------------------------------------

test_that("completeColumns treats NaN as missing", {
  
  x <- list(
    a = c(1, 2),
    b = c(1, NaN)
  )
  
  expect_equal(completeColumns(x), "a")
})

test_that("completeColumns treats NULL elements correctly", {
  
  x <- list(
    a = NULL,
    b = 1:3
  )
  
  expect_equal(completeColumns(x), c("a", "b"))
})

# -------------------------------------------------------------------------
# input validation
# -------------------------------------------------------------------------

test_that("completeColumns errors on invalid input", {
  
  expect_error(completeColumns(1:10))
  expect_error(completeColumns(NULL))
})

