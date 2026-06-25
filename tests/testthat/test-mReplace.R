library(testthat)

test_that("replaces matching values correctly", {
  x <- c("a", "b", "c", "d")
  
  expect_equal(
    mReplace(x, c("a", "c"), c("A", "C")),
    c("A", "b", "C", "d")
  )
})

test_that("returns unchanged vector when no matches exist", {
  x <- c("a", "b", "c")
  
  expect_equal(
    mReplace(x, c("x", "y"), c("X", "Y")),
    x
  )
})

test_that("replaces all occurrences of matching values", {
  x <- c("a", "b", "a", "c", "a")
  
  expect_equal(
    mReplace(x, c("a"), c("A")),
    c("A", "b", "A", "c", "A")
  )
})

test_that("works with empty input vector", {
  expect_equal(
    mReplace(character(0), c("a"), c("A")),
    character(0)
  )
})

test_that("works with empty replacement table", {
  x <- c("a", "b", "c")
  
  expect_equal(
    mReplace(x, character(0), character(0)),
    x
  )
})

test_that("handles NA values in x", {
  x <- c("a", NA, "b")
  
  expect_equal(
    mReplace(x, c("a", "b"), c("A", "B")),
    c("A", NA, "B")
  )
})

test_that("can replace NA when included in patterns", {
  x <- c("a", NA, "b")
  
  expect_equal(
    mReplace(x, c("a", NA), c("A", "Missing")),
    c("A", "Missing", "b")
  )
})

test_that("throws error when patterns and replacements differ in length", {
  expect_error(
    mReplace(c("a", "b"), c("a", "b"), c("A")),
    class = "simpleError"
  )
})

test_that("preserves vector length", {
  x <- c("a", "b", "c", "d")
  
  expect_length(
    mReplace(x, c("a", "c"), c("A", "C")),
    length(x)
  )
})

test_that("returns character vector", {
  expect_type(
    mReplace(c("a", "b"), c("a"), c("A")),
    "character"
  )
})