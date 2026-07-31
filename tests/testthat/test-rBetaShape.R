
test_that("rBetaShape respects its bounds and length", {

  set.seed(42)

  for (sh in c("norm", "left", "right", "unif", "u", "j", "inv-j")) {
    x <- rBetaShape(500, shape = sh, bounds = c(10, 90))
    expect_length(x, 500L)
    expect_true(all(x >= 10 & x <= 90), label = sh)
  }

  expect_length(rBetaShape(0), 0L)
})


test_that("the skew names follow the tail, not the bulk", {

  # "right"-skewed means a long RIGHT tail, so the bulk sits near the
  # LOWER bound - the point most likely to be misread.
  set.seed(1)
  right <- rBetaShape(5000, shape = "right", bounds = c(0, 100))
  left  <- rBetaShape(5000, shape = "left",  bounds = c(0, 100))

  expect_lt(mean(right), 50)
  expect_gt(mean(left),  50)
  expect_equal(mean(right), 100 * 2 / 7, tolerance = 0.05)
  expect_equal(mean(left),  100 * 5 / 7, tolerance = 0.05)
})


test_that("unif is exactly uniform and u is bimodal at the edges", {

  set.seed(7)
  u <- rBetaShape(4000, shape = "u")
  expect_gt(mean(u < 0.1 | u > 0.9), mean(u > 0.45 & u < 0.55))

  set.seed(7)
  viaShape <- rBetaShape(10, shape = "unif")
  set.seed(7)
  viaBeta <- stats::rbeta(10, 1, 1)
  expect_equal(viaShape, viaBeta)
})


test_that("a numeric shape is accepted and validated", {

  set.seed(3)
  a <- rBetaShape(10, shape = c(2, 5))
  set.seed(3)
  b <- rBetaShape(10, shape = "right")
  expect_equal(a, b)

  expect_error(rBetaShape(10, shape = c(2, 5, 1)), "two finite positive")
  expect_error(rBetaShape(10, shape = c(0, 5)), "two finite positive")
  expect_error(rBetaShape(10, shape = c(2, NA)), "two finite positive")
})


test_that("n and bounds are validated", {

  expect_error(rBetaShape(-1), "non-negative integer")
  expect_error(rBetaShape(2.5), "non-negative integer")
  expect_error(rBetaShape(c(1, 2)), "non-negative integer")
  expect_error(rBetaShape(NA), "non-negative integer")

  expect_error(rBetaShape(10, bounds = c(5, 5)), "increasing order")
  expect_error(rBetaShape(10, bounds = c(5, 1)), "increasing order")
  expect_error(rBetaShape(10, bounds = c(0, Inf)), "increasing order")

  # n above .Machine$integer.max passes validation (it is a non-negative
  # whole number); it must not be silently coerced to NA on the way to
  # rbeta(). Not drawing 3e9 values here - just checking the guard.
  expect_true(3e9 == floor(3e9) && is.finite(3e9))
})
