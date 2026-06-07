
# ── coalesceX ─────────────────────────────────────────────────────────────────
test_that("coalesceX returns first non-NA in vector", {
  expect_equal(coalesceX(c(NA, NA, NA, 5, 3)), 5)
})

test_that("coalesceX works with multiple vectors", {
  expect_equal(coalesceX(c(NA, 2), c(1, NA)), c(1, 2))
})

test_that("coalesceX works with data.frame", {
  d <- data.frame(a = c(NA, 2), b = c(1, NA))
  expect_equal(coalesceX(d), c(1, 2))
})

test_that("coalesceX method is.finite skips Inf", {
  expect_equal(coalesceX(c(Inf, NA, 3), c(1, 2, 4), method = "is.finite"), c(1, 2, 3))
})

test_that("coalesceX handles characters", {
  expect_equal(coalesceX(c(NA, "b"), c("a", NA)), c("a", "b"))
})


test_that("coalesceX method is.null skips NULL elements", {
  res <- coalesceX(NULL, 5, 3)
  expect_equal(res, 5)
})

test_that("coalesceX works with matrix input", {
  m   <- matrix(c(NA, 2, 1, NA), nrow = 2)
  res <- coalesceX(m)
  expect_equal(res, c(1, 2))
})

test_that("coalesceX works with list input", {
  res <- coalesceX(list(c(NA, 2), c(1, NA)))
  expect_equal(res, c(1, 2))
})

test_that("coalesceX method is.finite skips NaN", {
  expect_equal(
    coalesceX(c(NaN, NA, 3), c(1, 2, 4), method = "is.finite"),
    c(1, 2, 3)
  )
})

