
# ── factorize ─────────────────────────────────────────────────────────────────
test_that("factorize returns list of correct length", {
  res <- factorize(c(12, 47))
  expect_equal(length(res), 2)
})

test_that("factorize of prime returns single factor with exponent 1", {
  res <- factorize(47)[[1]]
  expect_equal(unname(res[, "p"]), 47)
  expect_equal(unname(res[, "m"]), 1)
})

test_that("factorize of 12 returns correct factors", {
  res <- factorize(12)[[1]]
  expect_equal(unname(res[, "p"]), c(2, 3))
  expect_equal(unname(res[, "m"]), c(2, 1))
})


test_that("factorize names result by input", {
  res <- factorize(c(6, 10))
  expect_equal(names(res), c("6", "10"))
})

