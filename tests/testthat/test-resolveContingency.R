
# ── resolveContingency ────────────────────────────────────────────────────────

test_that("resolveContingency works with matrix input", {
  m <- matrix(c(10, 5, 3, 8), nrow = 2)
  res <- resolveContingency(m)
  expect_equal(res$n, 26)
  expect_equal(res$r, 2L)
  expect_equal(res$c, 2L)
})

test_that("resolveContingency works with two vectors", {
  x <- c("A","A","B","B")
  y <- c("X","Y","X","Y")
  res <- resolveContingency(x, y)
  expect_equal(res$r, 2L)
  expect_equal(res$c, 2L)
})

test_that("resolveContingency stops on non-numeric matrix", {
  m <- matrix(c("a","b","c","d"), nrow = 2)
  expect_error(resolveContingency(m), "numeric")
})

test_that("resolveContingency warns on non-integer counts", {
  m <- matrix(c(1.5, 2.5, 3.5, 4.5), nrow = 2)
  expect_warning(resolveContingency(m), "non-integer")
})

test_that("resolveContingency stops if not square when square=TRUE", {
  m <- matrix(c(1,2,3,4,5,6), nrow = 2)
  expect_error(resolveContingency(m, square = TRUE), "square")
})


