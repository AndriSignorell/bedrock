

# -------------------------------------------------------------------
# tests for unwhich()
# -------------------------------------------------------------------

test_that("basic reconstruction works", {
  
  expect_equal(
    unwhich(c(1, 3, 5), n = 5),
    c(TRUE, FALSE, TRUE, FALSE, TRUE)
  )
  
})


test_that("default n uses max(idx)", {
  
  expect_equal(
    unwhich(c(2, 4)),
    c(FALSE, TRUE, FALSE, TRUE)
  )
  
})


test_that("empty idx returns all FALSE", {
  
  expect_equal(
    unwhich(integer(0), n = 5L),
    rep(FALSE, 5)
  )
  
  expect_length(
    unwhich(integer(0)),
    0L
  )
  
})


test_that("duplicate indices are ignored", {
  
  expect_equal(
    unwhich(c(1, 1, 3, 3), n = 4),
    c(TRUE, FALSE, TRUE, FALSE)
  )
  
})



test_that("names are omitted when useNames = FALSE", {
  
  idx <- c(a = 1, b = 3)
  
  res <- unwhich(idx, n = 4, useNames = FALSE)
  
  expect_null(names(res))
  
})


test_that("unnamed idx produces unnamed result", {
  
  res <- unwhich(c(1, 3), n = 4)
  
  expect_null(names(res))
  
})


test_that("n must be a non-negative whole number", {
  
  expect_error(
    unwhich(1, n = -1),
    "non-negative whole number"
  )
  
  expect_error(
    unwhich(1, n = 1.5),
    "non-negative whole number"
  )
  
  expect_error(
    unwhich(1, n = NA),
    "non-negative whole number"
  )
  
})

test_that("idx validation: non-whole numbers raise error", {
  expect_error(unwhich(c(1, 2.5), n = 5), "non-zero whole numbers")
})

test_that("idx validation: NA raises error", {
  expect_error(unwhich(c(1L, NA_integer_), n = 5), "non-zero whole numbers")
})

test_that("idx validation: zero raises error", {
  expect_error(unwhich(c(0L, 1L), n = 5), "non-zero whole numbers")
})

test_that("idx validation: mixed positive and negative raises error", {
  expect_error(unwhich(c(-1L, 2L), n = 5), "must not mix")
})


test_that("n cannot be smaller than max(idx)", {
  
  expect_error(
    unwhich(c(2, 5), n = 4),
    "must not be less than max"
  )
  
})


test_that("works with integer input", {
  
  expect_equal(
    unwhich(c(1L, 4L), n = 5L),
    c(TRUE, FALSE, FALSE, TRUE, FALSE)
  )
  
})


test_that("logical result has correct length and type", {
  
  res <- unwhich(c(2, 4), n = 6)
  
  expect_type(res, "logical")
  expect_length(res, 6L)
  
})