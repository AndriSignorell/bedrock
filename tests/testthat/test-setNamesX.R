library(testthat)

test_that("unnamed argument defaults to names", {
  x <- setNamesX(1:3, letters[1:3])
  expect_equal(names(x), c("a", "b", "c"))
})

test_that("explicit names argument works", {
  x <- setNamesX(1:3, names = letters[1:3])
  expect_equal(names(x), c("a", "b", "c"))
})

test_that("rownames and colnames can be set together", {
  m <- setNamesX(matrix(1:12, nrow = 4),
                 rownames = LETTERS[1:4], colnames = c("x", "y", "z"))
  expect_equal(rownames(m), LETTERS[1:4])
  expect_equal(colnames(m), c("x", "y", "z"))
})

test_that("mixed unnamed and named arguments work", {
  # regression: unnamed arg mixed with a named one must default to 'names'
  x <- setNamesX(1:3, letters[1:3], rownames = NULL)
  expect_equal(names(x), c("a", "b", "c"))
})

test_that("names are recycled", {
  m <- setNamesX(diag(4), rownames = "", colnames = "")
  expect_equal(rownames(m), rep("", 4))
  expect_equal(colnames(m), rep("", 4))
})

test_that("rownames = NULL removes existing rownames", {
  m <- matrix(1:4, 2, dimnames = list(c("a", "b"), c("x", "y")))
  m <- setNamesX(m, rownames = NULL)
  expect_null(rownames(m))
  expect_equal(colnames(m), c("x", "y"))
})

test_that("dimnames can be set", {
  tab <- setNamesX(as.table(rbind(c(1, 2), c(3, 4))),
                   dimnames = list(a = c("x", "y"), b = c("u", "v")))
  expect_equal(names(dimnames(tab)), c("a", "b"))
})

test_that("abbreviations are supported", {
  m <- setNamesX(matrix(1:4, 2), rown = c("a", "b"))
  expect_equal(rownames(m), c("a", "b"))
})

test_that("unknown argument name errors", {
  expect_error(setNamesX(1:3, foo = letters[1:3]))
})
