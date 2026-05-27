# =========================================================
# test-strSplitToCol.R
# =========================================================

library(testthat)

test_that("strSplitToCol splits vectors", {
  
  x <- c("A B C", "D E", "F")
  
  res <- strSplitToCol(x)
  
  expect_true(is.data.frame(res))
  expect_equal(nrow(res), 3)
  expect_equal(ncol(res), 3)
  
})

test_that("strSplitToCol supports custom delimiter", {
  
  x <- c("A|B|C", "D|E")
  
  res <- strSplitToCol(x, split = "|")
  
  expect_equal(res[1, 1], "A")
  
})

test_that("strSplitToCol pads shorter rows", {
  
  x <- c("A B", "C")
  
  res <- strSplitToCol(x)
  
  expect_equal(res[2, 2], "")
  
})

test_that("strSplitToCol stores cols attribute", {
  
  x <- c("A B", "C")
  
  res <- strSplitToCol(x)
  
  expect_true(!is.null(attr(res, "cols")))
  
})

test_that("strSplitToCol works with multiple columns", {
  
  df <- data.frame(
    a = c("x y", "z"),
    b = c("1 2", "3"),
    stringsAsFactors = FALSE
  )
  
  res <- strSplitToCol(df)
  
  expect_true(is.data.frame(res))
  
})

