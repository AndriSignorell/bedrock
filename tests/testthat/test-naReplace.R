library(testthat)

test_that("naReplace replaces numeric NA", {

  expect_equal(
    naReplace(c(1,NA,3), 0),
    c(1,0,3)
  )
})

test_that("naReplace works for factors", {

  x <- factor(c("a", NA))

  res <- naReplace(x, "missing")

  expect_true("missing" %in% levels(res))
})

test_that("naReplace works for ordered factors", {

  x <- ordered(c("low", NA), levels=c("low","high"))

  res <- naReplace(x, "medium")

  expect_true(is.ordered(res))
})

test_that("naReplace fills with existing level without warning", {

  x <- factor(c("a", NA))

  expect_no_warning(res <- naReplace(x, "a"))
  expect_equal(as.character(res), c("a", "a"))
  # no duplicate level introduced
  expect_equal(levels(res), "a")
})
