library(testthat)

test_that("sampleX samples the requested size", {
  set.seed(1)
  res <- sampleX(1:10, size = 5)
  expect_length(res, 5L)
  expect_true(all(res %in% 1:10))
})

test_that("missing size yields a permutation, like sample(x)", {
  set.seed(1)
  res <- sampleX(1:10)
  expect_length(res, 10L)
  expect_setequal(res, 1:10)
})

test_that("replace = TRUE allows size > length(x)", {
  set.seed(1)
  res <- sampleX(1:3, size = 10, replace = TRUE)
  expect_length(res, 10L)
})

test_that("data frame rows are sampled", {
  set.seed(1)
  res <- sampleX(mtcars, size = 5)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 5L)
  expect_equal(ncol(res), ncol(mtcars))
})

test_that("data frame default size permutes all rows", {
  set.seed(1)
  res <- sampleX(mtcars)
  expect_equal(nrow(res), nrow(mtcars))
  expect_setequal(rownames(res), rownames(mtcars))
})

test_that("single-row result stays a data frame", {
  res <- sampleX(mtcars, size = 1)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1L)
})
