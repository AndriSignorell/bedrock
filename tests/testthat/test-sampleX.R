library(testthat)

test_that("sampleX works for vectors", {
  set.seed(1)
  res <- sampleX(1:10, 5)

  expect_length(res, 5)
})

test_that("sampleX works for data frames", {
  set.seed(1)

  df <- data.frame(a=1:10)

  res <- sampleX(df, 5)

  expect_true(is.data.frame(res))
  expect_equal(nrow(res), 5)
})

test_that("sampleX supports replacement", {
  set.seed(1)

  res <- sampleX(1:3, 10, replace=TRUE)

  expect_length(res, 10)
})
