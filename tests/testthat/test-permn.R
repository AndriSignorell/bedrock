

# tests/testthat/test-permn.R

test_that("permn basic permutations without duplicates", {
  x <- c(1, 2, 3)
  res <- permn(x)
  
  expect_true(is.matrix(res))
  expect_equal(nrow(res), factorial(3))
  expect_equal(ncol(res), length(x))
  
  # jede Zeile ist Permutation von x
  for (i in seq_len(nrow(res))) {
    expect_equal(sort(res[i, ]), sort(x))
  }
})


test_that("permn handles duplicates correctly", {
  x <- c(1, 1, 2)
  res <- permn(x)
  
  # Anzahl: 3! / 2! = 3
  expect_equal(nrow(res), 3)
  
  expected <- rbind(
    c(1, 1, 2),
    c(1, 2, 1),
    c(2, 1, 1)
  )
  
  expect_true(all(apply(res, 1, function(r)
    any(apply(expected, 1, function(e) identical(r, e)))
  )))
})


test_that("permn returns correct result for single value", {
  x <- c(5, 5, 5)
  res <- permn(x)
  
  expect_equal(nrow(res), 1)
  expect_equal(res, matrix(x, nrow = 1))
})


test_that("permn handles length 1 input", {
  x <- 42
  res <- permn(x)
  
  expect_equal(nrow(res), 1)
  expect_equal(ncol(res), 1)
  expect_equal(res[1, 1], 42)
})


test_that("permn handles empty input", {
  x <- numeric(0)
  res <- permn(x)
  
  expect_equal(nrow(res), 1)
  expect_equal(ncol(res), 0)
})


test_that("permn preserves names", {
  x <- c(a = 1, b = 2, c = 3)
  res <- permn(x)
  
  expect_equal(colnames(res), names(x))
})


test_that("permn rejects non-atomic input", {
  x <- list(1, 2, 3)
  
  expect_error(permn(x), "'x' must be an atomic vector")
})


test_that("permn rejects NA values", {
  x <- c(1, NA, 2)
  
  expect_error(permn(x), "missing values are not supported")
})



test_that("permn sortResults applies sortX", {
  x <- c(2, 1, 1)
  
  res_unsorted <- permn(x, sortResults = FALSE)
  res_sorted   <- permn(x, sortResults = TRUE)
  
  expect_equal(res_sorted, sortX(res_unsorted))
})


test_that("permn works for character input", {
  x <- c("a", "b", "a")
  res <- permn(x)
  
  expect_equal(nrow(res), 3)
  
  for (i in seq_len(nrow(res))) {
    expect_equal(sort(res[i, ]), sort(x))
  }
})


test_that("permn works for logical input", {
  x <- c(TRUE, FALSE, TRUE)
  res <- permn(x)
  
  expect_equal(nrow(res), 3)
  
  for (i in seq_len(nrow(res))) {
    expect_equal(sort(res[i, ]), sort(x))
  }
})


test_that("permn detects too many permutations", {
  x <- rep(1:10, each = 2)  # sehr gross
  
  expect_error(
    permn(x),
    "too many permutations"
  )
})


test_that("permn results are unique rows", {
  x <- c(1, 1, 2, 2)
  res <- permn(x)
  
  expect_equal(nrow(res), nrow(unique(res)))
})

