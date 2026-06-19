
library(testthat)

test_that("nUnique counts unique values", {
  
  expect_equal(
    nUnique(c(1,1,2,3)),
    3
  )
})

test_that("nUnique handles NA", {
  
  expect_equal(
    nUnique(c(1,1,NA)),
    2
  )
})

test_that("nUnique removes NA", {
  
  expect_equal(
    nUnique(c(1,1,NA), na.rm=TRUE),
    1
  )
})

test_that("nUnique handles all-NA vector", {
  
  expect_equal(
    nUnique(c(NA, NA, NA)),
    1
  )
  
  expect_equal(
    nUnique(c(NA, NA, NA), na.rm=TRUE),
    0
  )
})

test_that("nUnique handles empty vector", {
  
  expect_equal(
    nUnique(numeric(0)),
    0
  )
})

test_that("nUnique works on integer input", {
  
  expect_equal(
    nUnique(c(1L, 1L, 2L, 3L)),
    3
  )
  
  expect_equal(
    nUnique(c(1L, 1L, NA_integer_), na.rm=TRUE),
    1
  )
})


test_that("isLowCardinality returns TRUE clearly below threshold", {
  
  expect_true(
    isLowCardinality(c(1,2,2,3,NA), maxUnique=12)
  )
})

test_that("isLowCardinality returns FALSE clearly above threshold", {
  
  expect_false(
    isLowCardinality(as.numeric(1:100), maxUnique=12)
  )
})

test_that("isLowCardinality returns TRUE exactly at threshold", {
  
  expect_true(
    isLowCardinality(1:12, maxUnique=12)
  )
  
  # duplicates should not inflate the count past the threshold
  expect_true(
    isLowCardinality(rep(1:12, 10), maxUnique=12)
  )
})

test_that("isLowCardinality returns FALSE one above threshold", {
  
  expect_false(
    isLowCardinality(1:13, maxUnique=12)
  )
})

test_that("isLowCardinality handles all-NA vector", {
  
  expect_true(
    isLowCardinality(c(NA_real_, NA_real_), maxUnique=12)
  )
})

test_that("isLowCardinality handles empty vector", {
  
  expect_true(
    isLowCardinality(numeric(0), maxUnique=12)
  )
})

test_that("isLowCardinality works on integer input", {
  
  expect_true(
    isLowCardinality(c(1L,2L,2L,3L,NA_integer_), maxUnique=12)
  )
  
  expect_false(
    isLowCardinality(1:13L, maxUnique=12)
  )
})

test_that("isLowCardinality uses default maxUnique of 12", {
  
  expect_true(
    isLowCardinality(1:12)
  )
  
  expect_false(
    isLowCardinality(1:13)
  )
})

test_that("isLowCardinality errors on unsupported input type", {
  
  expect_error(
    isLowCardinality(c("a","b","c"), maxUnique=12)
  )
})