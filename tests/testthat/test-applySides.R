test_that("two.sided clamps but opens nothing", {

  expect_equal(applySides(c(0.2, 0.6), "two.sided", 0, 1),
               c(lci = 0.2, uci = 0.6))

  # the two-sided interval is clamped too - an interval must not claim a
  # value the statistic cannot take
  expect_equal(applySides(c(-0.2, 1.3), "two.sided", 0, 1),
               c(lci = 0, uci = 1))

  expect_equal(applySides(c(-3, 4), "two.sided", -1, 1),
               c(lci = -1, uci = 1))
})


test_that("sides names the side carrying the finite bound", {

  ci <- c(0.2, 0.6)

  # "left" keeps the lower bound and opens upwards - the analogue of
  # alternative = "greater"
  expect_equal(applySides(ci, "left", 0, 1),  c(lci = 0.2, uci = 1))
  expect_equal(applySides(ci, "right", 0, 1), c(lci = 0,   uci = 0.6))

  expect_equal(applySides(ci, "left", -Inf, Inf),  c(lci = 0.2, uci = Inf))
  expect_equal(applySides(ci, "right", -Inf, Inf), c(lci = -Inf, uci = 0.6))
})


test_that("a range may be bounded on one end only", {

  # Cronbach's alpha
  expect_equal(applySides(c(0.61, 0.94), "right", -Inf, 1),
               c(lci = -Inf, uci = 0.94))
  expect_equal(applySides(c(0.61, 0.94), "left", -Inf, 1),
               c(lci = 0.61, uci = 1))

  # a relative risk
  expect_equal(applySides(c(0.8, 2.4), "left", 0, Inf),
               c(lci = 0.8, uci = Inf))
  expect_equal(applySides(c(0.8, 2.4), "right", 0, Inf),
               c(lci = 0, uci = 2.4))
})


test_that("NA bounds pass through untouched", {

  # an interval that could not be computed stays undefined; the ordering
  # check must not fire on it
  expect_equal(applySides(c(NA_real_, NA_real_), "two.sided", -1, 1),
               c(lci = NA_real_, uci = NA_real_))

  # the open side is still closed at the boundary
  res <- applySides(c(NA_real_, NA_real_), "left", -1, 1)
  expect_true(is.na(res[["lci"]]))
  expect_equal(res[["uci"]], 1)

  res <- applySides(c(NA_real_, NA_real_), "right", 0, Inf)
  expect_equal(res[["lci"]], 0)
  expect_true(is.na(res[["uci"]]))

  expect_silent(applySides(c(NaN, NaN), "two.sided", 0, 1))

  # c(NA, NA) is LOGICAL, not numeric - and it is what a caller writes for
  # an interval that could not be computed. Rejecting it on is.numeric()
  # was the same mistake as testing the type of conf.level before
  # admitting NA.
  expect_equal(applySides(c(NA, NA), "two.sided", -1, 1),
               c(lci = NA_real_, uci = NA_real_))
  expect_equal(applySides(c(NA, NA), "left", -1, 1)[["uci"]], 1)

  # a partially missing interval works either way, because c() coerces
  expect_true(is.na(applySides(c(NA, 0.5), "two.sided", 0, 1)[["lci"]]))

  # but a genuine logical vector is still refused
  expect_error(applySides(c(TRUE, FALSE), "two.sided", 0, 1), "numeric")
})


test_that("the result is always named and unnamed on input", {

  res <- applySides(c(lower = 0.2, upper = 0.6), "two.sided", 0, 1)
  expect_named(res, c("lci", "uci"))
  expect_null(names(unname(res)))

  # and the order is lci, uci
  expect_equal(unname(res), c(0.2, 0.6))
})


test_that("invalid input is refused", {

  expect_error(applySides(0.5, "two.sided", 0, 1), "length 2")
  expect_error(applySides(c(1, 2, 3), "two.sided", 0, 1), "length 2")
  expect_error(applySides("a", "two.sided", 0, 1), "numeric")

  expect_error(applySides(c(0.6, 0.2), "two.sided", 0, 1), "that order")

  expect_error(applySides(c(0.2, 0.6), "two.sided", 1, 0), "range")
  expect_error(applySides(c(0.2, 0.6), "two.sided", NA, 1), "range")
  expect_error(applySides(c(0.2, 0.6), "two.sided", c(0, 0), 1), "range")

  # not matched here: callers resolve 'sides' with match.arg first, so an
  # unmatched value is a caller bug and must not be guessed at
  expect_error(applySides(c(0.2, 0.6), "l", 0, 1), "two.sided")
  expect_error(applySides(c(0.2, 0.6), "greater", 0, 1), "two.sided")
})


test_that("the defaults are the unbounded case", {

  expect_equal(applySides(c(-1.4, 2.6)), c(lci = -1.4, uci = 2.6))
  expect_equal(applySides(c(-1.4, 2.6), "left"), c(lci = -1.4, uci = Inf))
})
