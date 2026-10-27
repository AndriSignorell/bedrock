test_that("ordered rules reproduce nested ifelse including NA propagation", {
  d <- expand.grid(pd = c(FALSE, TRUE, NA), rls = c(FALSE, TRUE, NA),
                   low = c(FALSE, TRUE, NA), pred = c(FALSE, TRUE, NA))
  expected <- with(d, ifelse(pd, 1L, ifelse(rls, 0L,
                   ifelse(low, 0L, ifelse(pred, 1L, 0L)))))
  actual <- with(d, caseWhen(pd ~ 1L, rls ~ 0L, low ~ 0L,
                             pred ~ 1L, default = 0L))
  expect_identical(actual, expected)
})

test_that("first matches and missing conditions have explicit precedence", {
  expect_identical(caseWhen(TRUE ~ 1L, TRUE ~ 2L), 1L)
  expect_identical(caseWhen(TRUE ~ 1L, NA ~ 2L), 1L)
  expect_identical(caseWhen(NA ~ 1L, TRUE ~ 2L), NA_integer_)
  expect_identical(caseWhen(NA ~ 1L, TRUE ~ 2L, na = "skip"), 2L)
  expect_identical(caseWhen(TRUE ~ NA_integer_, TRUE ~ 2L), NA_integer_)
  expect_identical(caseWhen(FALSE ~ 1L), NA_integer_)
})

test_that("lengths are strict and empty vectors are supported", {
  expect_identical(caseWhen(c(TRUE, FALSE) ~ 3L, default = 0L), c(3L, 0L))
  expect_identical(caseWhen(TRUE ~ 1:3), 1:3)
  expect_identical(caseWhen(FALSE ~ 9L, default = 1:3), 1:3)
  expect_identical(caseWhen(logical() ~ 1L), integer())
  expect_identical(caseWhen(TRUE ~ integer()), integer())
  expect_error(caseWhen(c(TRUE, FALSE) ~ 1:3), "length")
  expect_error(caseWhen(logical() ~ 1:2), "length")
  expect_error(caseWhen(TRUE ~ 1:2, default = 1:4), "length")
})

test_that("types are predictable and names are discarded", {
  expect_identical(caseWhen(TRUE ~ 1L, FALSE ~ 2), 1)
  expect_identical(caseWhen(FALSE ~ NA, TRUE ~ "x"), "x")
  expect_identical(caseWhen(FALSE ~ "x"), NA_character_)
  expect_identical(caseWhen(FALSE ~ 1i), NA_complex_)
  expect_identical(caseWhen(TRUE ~ 1L, default = 1i), 1 + 0i)
  expect_identical(caseWhen(TRUE ~ NA), NA)
  expect_identical(caseWhen(c(a = TRUE, b = FALSE) ~ c(x = 1L, y = 2L),
                           default = c(z = 0L)), c(1L, 0L))
  expect_error(caseWhen(TRUE ~ 1L, FALSE ~ "x"), "cannot be mixed")
  expect_error(caseWhen(TRUE ~ "x", default = FALSE), "cannot be mixed")
})

test_that("unsupported inputs fail clearly", {
  expect_error(caseWhen(), "at least one")
  expect_error(caseWhen(~ 1), "two-sided")
  expect_error(caseWhen(1), "two-sided")
  expect_error(caseWhen(1 ~ 2), "logical")
  expect_error(caseWhen(TRUE ~ NULL), "atomic")
  expect_error(caseWhen(TRUE ~ factor("a")), "atomic")
  expect_error(caseWhen(TRUE ~ as.Date("2026-01-01")), "atomic")
  expect_error(caseWhen(TRUE ~ list(1)), "atomic")
  expect_error(caseWhen(TRUE ~ matrix(1)), "atomic")
  expect_error(caseWhen(TRUE ~ 1, default = list(0)), "default")
  expect_error(caseWhen(TRUE ~ 1, defalt = 0), "unnamed")
  expect_error(caseWhen(TRUE ~ 1, na = "other"), "propagate")
})

test_that("formulas use their own environments and all RHS are evaluated", {
  rule <- local({ threshold <- 2; c(1, 3) > threshold ~ 7L })
  expect_identical(caseWhen(rule, default = 0L), c(0L, 7L))
  expect_error(caseWhen(TRUE ~ 1L, FALSE ~ stop("unused RHS")), "unused RHS")
  count <- 0L
  result <- caseWhen(TRUE ~ { count <- count + 1L; 4L })
  expect_identical(count, 1L)
  expect_identical(result, 4L)
})


test_that("skip uses default when all conditions are missing", {
  expect_identical(caseWhen(c(NA, NA) ~ 1L, NA ~ 2L,
                           default = 0L, na = "skip"), c(0L, 0L))
})

test_that("empty conditions accept a scalar default", {
  expect_identical(caseWhen(logical() ~ 1L, default = 0L), integer())
})

test_that("a missing condition takes precedence over default", {
  expect_identical(caseWhen(NA ~ 1L, default = 0L), NA_integer_)
})

test_that("complex missing values are canonical NA_complex_", {
  # as.complex(NA) is NA+0i since R 4.4.0
  expect_identical(caseWhen(FALSE ~ 1i), NA_complex_)
  expect_identical(caseWhen(TRUE ~ NA, default = 1i), NA_complex_)
  expect_identical(caseWhen(TRUE ~ c(NA, 1L), default = 1i),
                   c(NA_complex_, 1 + 0i))
})
