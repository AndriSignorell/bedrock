library(testthat)

test_that("label gets and sets atomic labels", {

  x <- 1:5
  label(x) <- "numbers"

  expect_equal(label(x), "numbers")
})

test_that("label gets and sets data frame labels", {

  df <- data.frame(a=1:3)

  label(df) <- "dataset"

  expect_equal(label(df), "dataset")
})

test_that("label sets variable labels", {

  df <- data.frame(a=1:3, b=4:6)

  label(df, vars=TRUE) <- c("A","B")

  expect_equal(
    label(df, vars=TRUE),
    c(a="A", b="B")
  )
})

test_that("label replicates scalar variable label", {

  df <- data.frame(a=1:3, b=4:6)

  label(df, vars=TRUE) <- "X"

  expect_equal(
    unname(label(df, vars=TRUE)),
    c("X","X")
  )
})

