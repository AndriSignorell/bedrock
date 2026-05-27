library(testthat)

test_that("dummy creates treatment contrasts", {
  x <- c("A", "B", "A")
  res <- dummy(x)

  expect_true(is.matrix(res))
  expect_equal(nrow(res), 3)
})

test_that("dummy full coding returns all levels", {
  x <- c("A", "B")
  res <- dummy(x, method="full")

  expect_equal(colnames(res), c("A", "B"))
})

test_that("dummy supports base by name", {
  x <- c("A", "B", "C")
  res <- dummy(x, base="B")

  expect_equal(attr(res, "base"), "B")
})

test_that("dummy errors for invalid base", {
  expect_error(dummy(c("A","B"), base=5))
})

test_that("dummy preserves row names", {
  x <- c(a="A", b="B")
  res <- dummy(x)

  expect_equal(rownames(res), c("a","b"))
})
