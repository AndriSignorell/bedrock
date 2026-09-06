library(testthat)

test_that("rankX computes ranks", {

  expect_equal(
    rankX(c(10,20,20,30)),
    c(1,2.5,2.5,4)
  )
})

test_that("rankX supports dense ranking", {

  expect_equal(
    rankX(c(1,2,2,3), ties.method="dense"),
    c(1,2,2,3)
  )
})

test_that("rankX supports decreasing order", {

  expect_equal(
    rankX(c(1,2,3), decreasing=TRUE),
    c(3,2,1)
  )
})

test_that("rankX errors for unequal lengths", {

  expect_error(
    rankX(1:3, 1:2)
  )
})



test_that("rankX agrees with base rank for common tie methods", {
  x <- c(30, 10, 20, 20, NA)
  
  for (method in c("average", "first", "max", "min")) {
    expect_equal(
      rankX(x, ties.method = method, na.last = "keep"),
      rank(x, ties.method = method, na.last = "keep"),
      info = paste("ties.method =", method)
    )
  }
})

test_that("rankX supports last and random tie handling", {
  x <- c(2, 1, 2, 1)
  
  expect_identical(rankX(x, ties.method = "last"), c(4L, 2L, 3L, 1L))
  
  set.seed(42)
  out <- rankX(x, ties.method = "random")
  expect_equal(sort(out), 1:4)
})

test_that("rankX ranks several vectors lexicographically", {
  a <- c(1, 1, 2, 2)
  b <- c(2, 1, 2, 1)
  
  expect_equal(rankX(a, b), c(2, 1, 4, 3))
  expect_equal(
    rankX(a, b, decreasing = c(FALSE, TRUE)),
    1:4
  )
})

test_that("rankX handles NA placement", {
  x <- c(3, NA, 1, 2)
  
  expect_equal(rankX(x, na.last = TRUE), rank(x, na.last = TRUE))
  expect_equal(rankX(x, na.last = FALSE), rank(x, na.last = FALSE))
  expect_equal(rankX(x, na.last = "keep"), rank(x, na.last = "keep"))
})

test_that("rankX validates its inputs", {
  expect_error(rankX(), "no input vectors")
  expect_error(rankX(1:3, decreasing = NA), "decreasing")
  expect_error(rankX(1:3, decreasing = 1), "decreasing")
  expect_error(rankX(1:3, ties.method = "unknown"), "arg")
})

