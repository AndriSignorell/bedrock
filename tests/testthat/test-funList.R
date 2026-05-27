library(testthat)

test_that("funList returns exported functions", {

  res <- funList("stats")

  expect_true(is.character(res))
  expect_true("lm" %in% res)
})

test_that("funList works with exported = FALSE", {

  res <- funList("stats", exported=FALSE)

  expect_true(is.character(res))
})
