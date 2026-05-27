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

test_that("naReplace warns for existing level", {

  x <- factor(c("a", NA))

  expect_warning(
    naReplace(x, "a")
  )
})
