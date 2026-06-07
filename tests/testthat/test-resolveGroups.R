
# ── resolveGroups ─────────────────────────────────────────────────────────────

test_that("resolveGroups works with vector and grouping", {
  x <- c(1, 2, 3, 4, 5, 6)
  g <- c("A","A","A","B","B","B")
  res <- resolveGroups(x, g)
  expect_equal(res$k, 2L)
  expect_equal(res$n, 6L)
  expect_equal(res$group.names, c("A","B"))
})

test_that("resolveGroups works with list input", {
  res <- resolveGroups(list(A = c(1,2,3), B = c(4,5,6)))
  expect_equal(res$k, 2L)
  expect_equal(res$group.names, c("A","B"))
})

test_that("resolveGroups removes NAs", {
  x <- c(1, NA, 3, 4, 5, 6)
  g <- c("A","A","A","B","B","B")
  res <- resolveGroups(x, g)
  expect_equal(res$n, 5L)
})

test_that("resolveGroups stops if g missing", {
  expect_error(resolveGroups(c(1,2,3)), "'g' is missing")
})

test_that("resolveGroups stops if only one group", {
  expect_error(resolveGroups(c(1,2,3), c("A","A","A")), "same group")
})

test_that("resolveGroups warns when list has non-numeric", {
  expect_warning(resolveGroups(list(A = c("x","y"), B = c("a","b"))),
                 "not numeric")
})