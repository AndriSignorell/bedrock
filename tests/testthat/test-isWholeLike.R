
test_that("isWholeLike works for integer vectors", {
  expect_true(isWholeLike(1:5))
  expect_equal(isWholeLike(1:5, all = FALSE), rep(TRUE, 5))
})

test_that("isWholeLike works for numeric vectors", {
  expect_true(isWholeLike(c(1, 2, 3)))
  expect_false(isWholeLike(c(1, 2.1, 3)))
  
  expect_equal(
    isWholeLike(c(1, 2.1, 3), all = FALSE),
    c(TRUE, FALSE, TRUE)
  )
})

test_that("tolerance works correctly", {
  x <- c(1, 2 + 1e-9)
  expect_true(isWholeLike(x, tol = 1e-6))
  expect_false(isWholeLike(x, tol = 1e-12))
})

test_that("nonNegative constraint works", {
  expect_true(isWholeLike(c(1, 2, 3), nonNegative = TRUE))
  expect_false(isWholeLike(c(1, -2, 3), nonNegative = TRUE))
})

test_that("NA handling works", {
  x <- c(1, NA, 2)
  
  expect_false(isWholeLike(x))
  expect_true(isWholeLike(x, na.rm = TRUE))
  
  expect_equal(
    isWholeLike(x, all = FALSE, na.rm = TRUE),
    c(TRUE, TRUE)
  )
})

test_that("complex numbers are handled correctly", {
  z1 <- c(1+0i, 2+0i)
  expect_true(isWholeLike(z1))
  
  z2 <- c(1+0i, 2+0.5i)
  expect_false(isWholeLike(z2))
  
  expect_equal(
    isWholeLike(z2, all = FALSE),
    c(TRUE, FALSE)
  )
})

test_that("non-numeric inputs return FALSE", {
  expect_false(isWholeLike("a"))
  expect_equal(isWholeLike("a", all = FALSE), FALSE)
})


test_that("empty vector behavior", {
  expect_true(isWholeLike(numeric(0)))
  expect_equal(isWholeLike(numeric(0), all = FALSE), logical(0))
})

test_that("integer shortcut works", {
  x <- as.integer(c(1, 2, 3))
  expect_true(isWholeLike(x))
})