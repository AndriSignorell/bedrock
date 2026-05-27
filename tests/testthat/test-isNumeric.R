
test_that("basic numeric validation works", {
  expect_true(isNumeric(c(1, 2, 3)))
  expect_false(isNumeric("a"))
  expect_false(isNumeric(list(1, 2)))
})

test_that("NA handling works correctly", {
  x <- c(1, NA, 2)
  
  expect_false(isNumeric(x))
  expect_true(isNumeric(x, na.rm = TRUE))
})

test_that("finite values are required", {
  expect_false(isNumeric(c(1, Inf)))
  expect_false(isNumeric(c(1, -Inf)))
  expect_false(isNumeric(c(1, NaN)))
})

test_that("isIntegerValued uses isWholeLike internally", {
  expect_true(isNumeric(c(1, 2, 3), isIntegerValued = TRUE))
  expect_false(isNumeric(c(1, 2.1, 3), isIntegerValued = TRUE))
})

test_that("tolerance is respected for isIntegerValued", {
  x <- c(1, 2 + 1e-9)
  
  expect_true(isNumeric(x,  isIntegerValued = TRUE, tol = 1e-6))
  expect_false(isNumeric(x, isIntegerValued = TRUE, tol = 1e-12))
})

test_that("isPositive constraint works", {
  expect_true(isNumeric(c(1, 2, 3),    isPositive = TRUE))
  expect_false(isNumeric(c(1, -2, 3),  isPositive = TRUE))
  expect_false(isNumeric(c(0, 1, 2),   isPositive = TRUE))
})

test_that("combined constraints work together", {
  expect_true(
    isNumeric(c(1, 2, 3),
              isIntegerValued = TRUE,
              isPositive      = TRUE)
  )
  
  expect_false(
    isNumeric(c(1, 2.1, 3),
              isIntegerValued = TRUE,
              isPositive      = TRUE)
  )
})

test_that("empty vector behavior", {
  expect_true(isNumeric(numeric(0)))
})

