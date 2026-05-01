
library(testthat)

# ---------------------------

# basic functionality

# ---------------------------

test_that("recycle works with different lengths", {
  res <- recycle(x = 1:5, y = 1, s = letters[1:2])
  
  expect_equal(length(res$x), 5)
  expect_equal(length(res$y), 5)
  expect_equal(length(res$s), 5)
  
  expect_equal(res$y, rep(1, 5))
  expect_equal(res$s, rep(letters[1:2], length.out = 5))
})

# ---------------------------

# maxdim inferred correctly

# ---------------------------

test_that("maxdim inferred from longest input", {
  res <- recycle(a = 1:3, b = 1:5)
  
  expect_equal(attr(res, "maxdim"), 5)
})

# ---------------------------

# explicit maxdim

# ---------------------------

test_that("explicit maxdim works", {
  res <- recycle(a = 1:2, maxdim = 6)
  
  expect_equal(length(res$a), 6)
  expect_equal(attr(res, "maxdim"), 6)
})

# ---------------------------

# strict mode: valid

# ---------------------------

test_that("strict mode allows length 1 or maxdim", {
  expect_silent(
    recycle(a = 1:5, b = 1, strict = TRUE)
  )
})

# ---------------------------

# strict mode: invalid

# ---------------------------

test_that("strict mode rejects invalid lengths", {
  expect_error(
    recycle(a = 1:5, b = 1:2, strict = TRUE)
  )
})

# ---------------------------

# maxdim validation

# ---------------------------

test_that("invalid maxdim throws error", {
  expect_error(recycle(a = 1:3, maxdim = -1))
  expect_error(recycle(a = 1:3, maxdim = c(1, 2)))
  expect_error(recycle(a = 1:3, maxdim = "a"))
})

# ---------------------------

# works with Dates

# ---------------------------

test_that("recycle works with Date objects", {
  d <- as.Date("2020-01-01") + 0:2
  
  res <- recycle(a = d, b = 1)
  
  expect_s3_class(res$a, "Date")
  expect_equal(length(res$a), 3)
  expect_equal(length(res$b), 3)
})

# ---------------------------

# names preserved

# ---------------------------

test_that("names are preserved", {
  res <- recycle(x = 1:3, y = 1)
  
  expect_true(all(c("x", "y") %in% names(res)))
})

# ---------------------------

# empty input

# ---------------------------

test_that("empty input returns empty list", {
  res <- recycle()
  
  expect_type(res, "list")
  expect_length(res, 0)
})

# ---------------------------

# attribute correctness

# ---------------------------

test_that("maxdim attribute is correct", {
  res <- recycle(a = 1:4, b = 1)
  
  expect_equal(attr(res, "maxdim"), 4)
})

# ---------------------------

# recycling behavior correctness

# ---------------------------

test_that("recycling repeats correctly", {
  res <- recycle(a = 1:2, maxdim = 5)
  
  expect_equal(res$a, c(1,2,1,2,1))
})

# ---------------------------

# multiple inputs consistency

# ---------------------------

test_that("all elements share same length", {
  res <- recycle(a = 1:3, b = 1, c = letters[1:2])
  
  lens <- lengths(res)
  expect_true(all(lens == attr(res, "maxdim")))
})
