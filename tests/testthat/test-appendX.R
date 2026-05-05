
library(testthat)

# -------------------------------------------------------------------------
# default method (vector)
# -------------------------------------------------------------------------

test_that("appendX.default behaves like base append", {
  x <- 1:5
  
  expect_equal(appendX(x, 99), append(x, 99))
  expect_equal(appendX(x, 99, after = 0), append(x, 99, after = 0))
  expect_equal(appendX(x, 99, after = 3), append(x, 99, after = 3))
})

test_that("appendX.default validates 'after'", {
  expect_error(appendX(1:5, 1, after = "a"))
  expect_error(appendX(1:5, 1, after = c(1,2)))
  expect_error(appendX(1:5, 1, after = -1))
  expect_error(appendX(1:5, 1, after = NA))
})

# -------------------------------------------------------------------------
# matrix: column insertion
# -------------------------------------------------------------------------

test_that("appendX.matrix inserts columns correctly", {
  x <- matrix(1:6, nrow = 3)
  
  res <- appendX(x, 7:9, after = 0)
  expect_equal(ncol(res), ncol(x) + 1)
  expect_equal(res[,1], 7:9)
  
  res <- appendX(x, 7:9, after = ncol(x))
  expect_equal(res[,ncol(res)], 7:9)
})


test_that("appendX.matrix column names are assigned", {
  x <- matrix(1:6, nrow = 3)
  
  res <- appendX(x, 7:9, newNames = "newcol")
  
  expect_equal(colnames(res)[ncol(res)], "newcol")
})


# -------------------------------------------------------------------------
# matrix: row insertion
# -------------------------------------------------------------------------

test_that("appendX.matrix inserts rows correctly", {
  x <- matrix(1:6, nrow = 3)
  
  res <- appendX(x, 10:11, rows = TRUE, after = 0)
  expect_equal(nrow(res), nrow(x) + 1)
  expect_equal(res[1,], c(10,11))
  
  res <- appendX(x, 10:11, rows = TRUE, after = nrow(x))
  expect_equal(res[nrow(res),], c(10,11))
})

test_that("appendX.matrix row names assigned", {
  x <- matrix(1:6, nrow = 3)
  
  res <- appendX(x, 10:11, rows = TRUE, newNames = "newrow")
  expect_true("newrow" %in% rownames(res))
})

# -------------------------------------------------------------------------
# data.frame: column insertion
# -------------------------------------------------------------------------

test_that("appendX.data.frame inserts columns correctly", {
  df <- data.frame(a = 1:3, b = 4:6)
  
  res <- appendX(df, c(7,8,9), newNames = "c")
  expect_true("c" %in% names(res))
  expect_equal(res$c, c(7,8,9))
})

test_that("appendX.data.frame respects insertion position", {
  df <- data.frame(a = 1:3, b = 4:6)
  
  res <- appendX(df, c(7,8,9), after = 0, newNames = "c")
  expect_equal(names(res)[1], "c")
})

# -------------------------------------------------------------------------
# data.frame: row insertion
# -------------------------------------------------------------------------

test_that("appendX.data.frame inserts rows correctly", {
  df <- data.frame(a = 1:3, b = 4:6)
  
  res <- appendX(df, c(10, 20),
                 rows = TRUE,
                 after = 0,
                 newNames = names(df))
  
  expect_equal(nrow(res), 4)
  expect_equal(as.numeric(res[1, ]), c(10, 20))
})



test_that("appendX.data.frame validates row structure", {
  df <- data.frame(a = 1:3, b = 4:6)
  
  expect_error(
    appendX(df, c(1,2,3), rows = TRUE),
    "same number of columns"
  )
})


# -------------------------------------------------------------------------
# recycling warnings
# -------------------------------------------------------------------------

test_that("appendX.matrix warns on improper recycling", {
  x <- matrix(1:6, nrow = 3)
  
  expect_warning(
    appendX(x, 1:5),
    "multiple"
  )
})

# -------------------------------------------------------------------------
# TOne class
# -------------------------------------------------------------------------

test_that("appendX.TOne preserves class and attributes", {
  x <- matrix(1:6, nrow = 3)
  class(x) <- "TOne"
  attr(x, "legend") <- "test"
  
  res <- appendX(x, 7:9)
  
  expect_equal(class(res), "TOne")
  expect_equal(attr(res, "legend"), "test")
})

# -------------------------------------------------------------------------
# edge cases
# -------------------------------------------------------------------------

test_that("after = NULL appends at end", {
  x <- 1:5
  expect_equal(appendX(x, 99), c(1:5, 99))
})

test_that("after = 0 prepends", {
  x <- 1:5
  expect_equal(appendX(x, 99, after = 0), c(99, 1:5))
})

test_that("single value insertion works", {
  x <- matrix(1:6, nrow = 3)
  
  res <- appendX(x, 99)
  expect_equal(ncol(res), 3)
})

