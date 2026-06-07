
library(testthat)

# ── as.matrix.xtabs / as.array.xtabs ─────────────────────────────────────────
test_that("as.matrix.xtabs drops xtabs and table class", {
  xt <- xtabs(~ cyl + gear, data = mtcars)
  m  <- as.matrix(xt)
  expect_true(is.matrix(m))
  expect_false(inherits(m, "xtabs"))
  expect_false(inherits(m, "table"))
})

test_that("as.matrix.xtabs preserves dimnames", {
  xt <- xtabs(~ cyl + gear, data = mtcars)
  m  <- as.matrix(xt)
  expect_equal(rownames(m), rownames(xt))
  expect_equal(colnames(m), colnames(xt))
})

test_that("as.array.xtabs drops call attribute", {
  xt <- xtabs(~ cyl + gear, data = mtcars)
  a  <- as.array(xt)
  expect_null(attr(a, "call"))
})

