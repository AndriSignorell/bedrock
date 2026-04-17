
library(testthat)

# =========================
# Vector tests
# =========================

test_that("sortX.default works like sort", {
  x <- c(3, 1, 2)
  expect_equal(sortX(x), sort(x))
})

test_that("mixed sorting works for character vectors", {
  x <- c("A1", "A10", "A2")
  expect_equal(sortX(x, method = "mixed"), c("A1", "A2", "A10"))
})

test_that("mixed sorting respects decreasing", {
  x <- c("A1", "A10", "A2")
  expect_equal(sortX(x, method = "mixed", decreasing = TRUE),
               c("A10", "A2", "A1"))
})

test_that("mixed sorting handles NA", {
  x <- c("A1", NA, "A2")
  expect_equal(sortX(x, method = "mixed", na.last = TRUE),
               c("A1", "A2", NA))
})

# =========================
# Data frame tests
# =========================

test_that("data.frame sorting default", {
  df <- data.frame(a = c(3,1,2), b = c("x","y","z"))
  res <- sortX(df, ord = "a")
  expect_equal(res$a, c(1,2,3))
})

test_that("data.frame mixed sorting", {
  df <- data.frame(a = c("A1","A10","A2"), b = 1:3)
  res <- sortX(df, ord = "a", method = "mixed")
  expect_equal(res$a, c("A1","A2","A10"))
})

test_that("data.frame multi-column sorting", {
  df <- data.frame(a = c("A1","A1","A2"),
                   b = c(2,1,1))
  
  res <- sortX(df, ord = c("a","b"), method = "mixed")
  
  expect_equal(res$a, c("A1","A1","A2"))
  expect_equal(res$b, c(1,2,1))
})

test_that("data.frame decreasing works", {
  df <- data.frame(a = c("A1","A10","A2"))
  
  res <- sortX(df, ord = "a", method = "mixed", decreasing = TRUE)
  
  expect_equal(res$a, c("A10","A2","A1"))
})

# =========================
# Matrix tests
# =========================

test_that("matrix sorting works", {
  m <- matrix(c(3,1,2, 9,8,7), ncol = 2)
  res <- sortX(m, ord = 1)
  expect_equal(res[,1], c(1,2,3))
})

test_that("matrix mixed sorting", {
  m <- cbind(c("A1","A10","A2"), 1:3)
  
  res <- sortX(m, ord = 1, method = "mixed")
  
  expect_equal(res[,1], c("A1","A2","A10"))
})

# =========================
# Table tests
# =========================

test_that("table sorting works", {
  tab <- HairEyeColor[,,1]
  
  res <- sortX(tab, ord = 2)
  
  expect_true(inherits(res, "table"))
})

test_that("table decreasing works", {
  tab <- HairEyeColor[,,1]
  
  res <- sortX(tab, ord = 2, decreasing = TRUE)
  
  expect_true(inherits(res, "table"))
})

# =========================
# Edge cases
# =========================

test_that("single element input works", {
  expect_equal(sortX("A1", method = "mixed"), "A1")
})

test_that("empty input works", {
  expect_equal(sortX(character(0), method = "mixed"), character(0))
})