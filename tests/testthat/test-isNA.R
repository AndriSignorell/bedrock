
library(testthat)

# ---------------------------

# TRUE cases

# ---------------------------

test_that("isNA returns TRUE for scalar NA values", {
  expect_true(isNA(NA))
  expect_true(isNA(NA_real_))
  expect_true(isNA(NA_integer_))
  expect_true(isNA(NA_character_))
})

# ---------------------------

# FALSE cases: non-NA

# ---------------------------

test_that("isNA returns FALSE for non-missing values", {
  expect_false(isNA(1))
  expect_false(isNA(0))
  expect_false(isNA("a"))
})

# ---------------------------

# FALSE cases: length > 1

# ---------------------------

test_that("isNA returns FALSE for vectors of length > 1", {
  expect_false(isNA(c(NA, NA)))
  expect_false(isNA(c(1, NA)))
})

# ---------------------------

# FALSE cases: NULL

# ---------------------------

test_that("isNA returns FALSE for NULL", {
  expect_false(isNA(NULL))
})

# ---------------------------

# FALSE cases: non-atomic

# ---------------------------

test_that("isNA returns FALSE for non-atomic objects", {
  expect_false(isNA(list(NA)))
  expect_false(isNA(data.frame(x = NA)))
})

# ---------------------------

# length zero

# ---------------------------

test_that("isNA returns FALSE for length-zero input", {
  expect_false(isNA(numeric(0)))
})
