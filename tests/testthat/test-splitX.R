library(testthat)

test_that("splitX default works", {

  x <- 1:4
  g <- c("a","a","b","b")

  res <- splitX(x, g)

  expect_equal(length(res), 2)
})

test_that("splitX formula works", {

  df <- data.frame(
    y=1:4,
    g=c("a","a","b","b")
  )

  res <- splitX(y ~ g, data=df)

  expect_equal(length(res), 2)
})

test_that("splitX formula errors for invalid formula", {

  expect_error(
    splitX(~ g, data=data.frame(g=1))
  )
})
