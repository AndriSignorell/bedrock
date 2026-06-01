
# =========================================================
# Tests for mergeArgs()
# =========================================================

library(testthat)

test_that("returns defaults if user is NULL", {
  
  defaults <- list(a = 1, b = 2)
  
  expect_equal(
    mergeArgs(defaults, NULL),
    defaults
  )
  
})


test_that("user arguments override defaults", {
  
  defaults <- list(a = 1, b = 2)
  user <- list(b = 99)
  
  expect_equal(
    mergeArgs(defaults, user),
    list(a = 1, b = 99)
  )
  
})


test_that("new user arguments are added", {
  
  defaults <- list(a = 1)
  user <- list(b = 2)
  
  expect_equal(
    mergeArgs(defaults, user),
    list(a = 1, b = 2)
  )
  
})


test_that("forbidden arguments are removed", {
  
  defaults <- list(a = 1)
  user <- list(b = 2, c = 3)
  
  res <- mergeArgs(
    defaults,
    user,
    forbidden = "b",
    warn = FALSE
  )
  
  expect_equal(
    res,
    list(a = 1, c = 3)
  )
  
})


test_that("warning is issued for forbidden arguments", {
  
  defaults <- list(a = 1)
  user <- list(b = 2)
  
  expect_warning(
    mergeArgs(
      defaults,
      user,
      forbidden = "b",
      warn = TRUE
    ),
    "Ignoring forbidden arguments"
  )
  
})


test_that("no warning is issued if warn = FALSE", {
  
  defaults <- list(a = 1)
  user <- list(b = 2)
  
  expect_no_warning(
    mergeArgs(
      defaults,
      user,
      forbidden = "b",
      warn = FALSE
    )
  )
  
})


test_that("multiple forbidden arguments are removed", {
  
  defaults <- list(a = 1)
  user <- list(b = 2, c = 3, d = 4)
  
  res <- mergeArgs(
    defaults,
    user,
    forbidden = c("b", "d"),
    warn = FALSE
  )
  
  expect_equal(
    res,
    list(a = 1, c = 3)
  )
  
})


test_that("nested lists are merged recursively", {
  
  defaults <- list(
    a = 1,
    opts = list(x = 10, y = 20)
  )
  
  user <- list(
    opts = list(y = 99)
  )
  
  expect_equal(
    mergeArgs(defaults, user),
    list(
      a = 1,
      opts = list(x = 10, y = 99)
    )
  )
  
})


test_that("empty user list leaves defaults unchanged", {
  
  defaults <- list(a = 1, b = 2)
  
  expect_equal(
    mergeArgs(defaults, list()),
    defaults
  )
  
})


test_that("works with forbidden = NULL", {
  
  defaults <- list(a = 1)
  user <- list(b = 2)
  
  expect_equal(
    mergeArgs(defaults, user, forbidden = NULL),
    list(a = 1, b = 2)
  )
  
})


test_that("works with empty forbidden vector", {
  
  defaults <- list(a = 1)
  user <- list(b = 2)
  
  expect_equal(
    mergeArgs(defaults, user, forbidden = character(0)),
    list(a = 1, b = 2)
  )
  
})


test_that("unnamed user list is accepted", {
  
  defaults <- list(a = 1)
  
  res <- mergeArgs(defaults, list(1, 2))
  
  expect_true(is.list(res))
  
})

