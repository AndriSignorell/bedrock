
# ── binaryTree ────────────────────────────────────────────────────────────────
test_that("binaryTree returns integer vector of length n", {
  res <- binaryTree(12)
  expect_equal(length(res), 12)
  expect_true(is.integer(res))
})

test_that("binaryTree contains all values 1:n", {
  n   <- 13
  res <- binaryTree(n)
  expect_equal(sort(res), 1:n)
})

test_that("binaryTree stops on invalid input", {
  expect_error(binaryTree(0))
  expect_error(binaryTree(-1))
  expect_error(binaryTree(c(1, 2)))
})


test_that("binaryTree returns the documented ordering", {
  expect_identical(binaryTree(1), 1L)
  expect_identical(
    binaryTree(13),
    c(8L, 4L, 9L, 2L, 10L, 5L, 11L, 1L, 12L, 6L, 13L, 3L, 7L)
  )
})

test_that("binaryTree rejects non-whole and irregular inputs", {
  expect_error(binaryTree(2.5), "whole number")
  expect_error(binaryTree(NA_real_), "positive integer")
  expect_error(binaryTree("3"), "positive integer")
  expect_error(binaryTree(numeric()), "positive integer")
})


