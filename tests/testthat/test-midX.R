
library(testthat)

# ---------------------------

# Basic functionality

# ---------------------------

test_that("basic midpoints are correct", {
  x <- c(1, 3, 6, 7)
  
  expect_equal(
    midx(x),
    c(2, 4.5, 6.5)
  )
})

# ---------------------------

# incl.zero

# ---------------------------

test_that("incl.zero prepends zero correctly", {
  x <- c(1, 3, 6, 7)
  
  expect_equal(
    midx(x, incl.zero = TRUE),
    c(0.5, 2, 4.5, 6.5)
  )
})

# ---------------------------

# cumulate

# ---------------------------

test_that("cumulate returns cumulative sum", {
  x <- c(1, 3, 6, 7)
  
  expect_equal(
    midx(x, cumulate = TRUE),
    cumsum(c(2, 4.5, 6.5))
  )
})

# ---------------------------

# incl.zero + cumulate

# ---------------------------

test_that("incl.zero and cumulate together", {
  x <- c(1, 3, 6, 7)
  
  expect_equal(
    midx(x, incl.zero = TRUE, cumulate = TRUE),
    cumsum(c(0.5, 2, 4.5, 6.5))
  )
})

# ---------------------------

# Length properties

# ---------------------------

test_that("output length is correct", {
  x <- rnorm(10)
  
  expect_length(midx(x), length(x) - 1)
  expect_length(midx(x, incl.zero = TRUE), length(x))
})

# ---------------------------

# Edge cases

# ---------------------------

test_that("edge cases: short vectors", {
  expect_equal(midx(numeric(0)), numeric(0))
  expect_equal(midx(5), numeric(0))
  
  expect_equal(midx(5, incl.zero = TRUE), 2.5)
})

# ---------------------------

# Numeric stability

# ---------------------------

test_that("works with negative and decimal values", {
  x <- c(-2, 0, 1.5, 4)
  
  expect_equal(
    midx(x),
    c(-1, 0.75, 2.75)
  )
})

# ---------------------------

# NA handling (propagates)

# ---------------------------

test_that("NA values propagate", {
  x <- c(1, NA, 3)
  
  res <- midx(x)
  
  expect_true(is.na(res[1]))
  expect_true(is.na(res[2]))
})

# ---------------------------

# Input validation

# ---------------------------

test_that("non-numeric input throws error", {
  expect_error(midx("a"))
  expect_error(midx(list(1,2)))
})

# ---------------------------

# Matrix / apply use case

# ---------------------------

test_that("works correctly with apply (matrix columns)", {
  tab <- matrix(c(1,2,3,4,5,6), nrow = 2)
  
  res <- t(apply(tab, 2, midx, incl.zero = TRUE, cumulate = TRUE))
  
  expect_true(is.matrix(res))
  expect_equal(ncol(res), nrow(tab))
})
