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

test_that("a single name is recycled", {
  m <- setNamesX(diag(4), rownames = "", colnames = "")
  expect_equal(rownames(m), rep("", 4))
  expect_equal(colnames(m), rep("", 4))

  expect_equal(names(setNamesX(1:3, "a")), rep("a", 3))
})

test_that("a deviating length is an error, not recycled", {
  m <- matrix(1:12, nrow = 4)
  expect_error(setNamesX(m, colnames = c("a", "b")), "not equal to extent")
  expect_error(setNamesX(m, rownames = c("a", "b")), "not equal to extent")
  expect_error(setNamesX(1:5, letters[1:2]), "not equal to extent")
  # too many names is an error as well
  expect_error(setNamesX(1:3, letters[1:4]), "not equal to extent")
  # so is a zero length vector
  expect_error(setNamesX(1:3, character(0)), "not equal to extent")
})

test_that("names of matching length pass through unchanged", {
  m <- setNamesX(matrix(1:12, nrow = 4), colnames = c("x", "y", "z"))
  expect_equal(colnames(m), c("x", "y", "z"))
  expect_null(rownames(m))
})

test_that("data frames use columns for names and rows for rownames", {
  d <- data.frame(a = 1:2, b = 3:4)
  d <- setNamesX(d, names = c("x", "y"), rownames = c("r1", "r2"))
  expect_equal(names(d), c("x", "y"))
  expect_equal(rownames(d), c("r1", "r2"))
  expect_error(setNamesX(d, names = c("x", "y", "z")), "not equal to extent")
})

test_that("setting rownames on an object without dimensions errors", {
  expect_error(setNamesX(1:3, rownames = "a"), "no dimensions")
  expect_error(setNamesX(1:3, colnames = "a"), "no dimensions")
})

test_that("setting colnames on a 1d array errors", {
  a <- array(1:3, dim = 3)
  expect_error(setNamesX(a, colnames = "a"), "less than two dimensions")
  # the first dimension does exist, so rownames go through
  expect_equal(rownames(setNamesX(a, rownames = c("a", "b", "c"))),
               c("a", "b", "c"))
})

test_that("rownames = NULL removes existing rownames", {
  m <- matrix(1:4, 2, dimnames = list(c("a", "b"), c("x", "y")))
  m <- setNamesX(m, rownames = NULL)
  expect_null(rownames(m))
  expect_equal(colnames(m), c("x", "y"))
})

test_that("names = NULL removes existing names", {
  x <- setNamesX(c(a = 1, b = 2), names = NULL)
  expect_null(names(x))
})

test_that("dimnames can be set", {
  tab <- setNamesX(as.table(rbind(c(1, 2), c(3, 4))),
                   dimnames = list(a = c("x", "y"), b = c("u", "v")))
  expect_equal(names(dimnames(tab)), c("a", "b"))
})

test_that("dimnames = NULL removes existing dimnames", {
  m <- matrix(1:4, 2, dimnames = list(c("a", "b"), c("x", "y")))
  expect_null(dimnames(setNamesX(m, dimnames = NULL)))
})

test_that("abbreviations are supported", {
  m <- setNamesX(matrix(1:4, 2), rown = c("a", "b"))
  expect_equal(rownames(m), c("a", "b"))
})

test_that("unknown argument name errors", {
  expect_error(setNamesX(1:3, foo = letters[1:3]))
})
