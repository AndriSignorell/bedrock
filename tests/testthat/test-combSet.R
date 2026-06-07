
# ── combSet ───────────────────────────────────────────────────────────────────
test_that("combSet returns correct number of unordered combinations", {
  res <- combSet(letters[1:4], 2, replace = FALSE, ordered = FALSE)
  expect_equal(nrow(res), choose(4, 2))
})

test_that("combSet ordered without replacement", {
  res <- combSet(letters[1:3], 2, replace = FALSE, ordered = TRUE)
  expect_equal(nrow(res), 3 * 2)  # permutations
})

test_that("combSet with replacement", {
  res <- combSet(letters[1:3], 2, replace = TRUE, ordered = FALSE)
  expect_equal(nrow(res), choose(3 + 2 - 1, 2))
})

test_that("combSet output=list returns list", {
  res <- combSet(letters[1:4], 2, output = "list")
  expect_true(is.list(res))
  expect_equal(length(res), choose(4, 2))
})

test_that("combSet with vector m returns list of matrices", {
  res <- combSet(letters[1:4], c(2, 3))
  expect_true(is.list(res))
  expect_equal(length(res), 2)
})

test_that("combSet ordered with replacement", {
  res <- combSet(letters[1:3], 2, replace = TRUE, ordered = TRUE)
  expect_equal(nrow(res), 3^2)
})

test_that("combSet list output with multiple m returns flat list", {
  res <- combSet(letters[1:4], c(2, 3), output = "list")
  expect_true(is.list(res))
  expect_equal(length(res), choose(4, 2) + choose(4, 3))
})

test_that("combSet preserves character values", {
  res <- combSet(c("x", "y", "z"), 2)
  expect_true(is.character(res))
})

