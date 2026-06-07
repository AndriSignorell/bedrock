
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


