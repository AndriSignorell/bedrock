library(testthat)

# ── setAttr ───────────────────────────────────────────────────────────────────

test_that("setAttr sets scalar attributes", {
  x <- setAttr(1:3, c("a", "b"), c("A", "B"))
  expect_equal(attr(x, "a"), "A")
  expect_equal(attr(x, "b"), "B")
})

test_that("single attribute takes a vector value", {
  x <- setAttr(1:10, "dim", c(2, 5))
  expect_equal(dim(x), c(2L, 5L))
})

test_that("list values allow non-scalar and mixed types", {
  x <- setAttr(1:10, c("dim", "myattr"), list(c(2, 5), "abc"))
  expect_equal(dim(x), c(2L, 5L))
  expect_equal(attr(x, "myattr"), "abc")
})

test_that("length mismatch errors", {
  expect_error(setAttr(1:3, c("a", "b"), list(1)), "same length")
})

test_that("non-character attrNames errors", {
  expect_error(setAttr(1:3, 1, "x"), "character")
})

# ── removeAttr ────────────────────────────────────────────────────────────────

test_that("removeAttr removes a single attribute", {
  x <- setAttr(1:3, c("a", "b"), c("A", "B"))
  x <- removeAttr(x, "a")
  expect_null(attr(x, "a"))
  expect_equal(attr(x, "b"), "B")
})

test_that("removeAttr without attrNames removes all attributes", {
  x <- setAttr(1:3, c("a", "b"), c("A", "B"))
  x <- removeAttr(x)
  expect_null(attributes(x))
})

# ── keepAttr ──────────────────────────────────────────────────────────────────

test_that("keepAttr keeps only the listed attributes", {
  x <- setAttr(1:3, c("a", "b"), c("A", "B"))
  x <- keepAttr(x, "a")
  expect_equal(names(attributes(x)), "a")
})
