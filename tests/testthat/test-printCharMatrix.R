library(testthat)

test_that("printCharMatrix runs invisibly", {

  m <- matrix(c("a","b","c","d"), nrow=2)

  expect_invisible(
    printCharMatrix(m)
  )
})

test_that("printCharMatrix supports left alignment", {

  m <- matrix(c("a","b"), nrow=1)

  expect_invisible(
    printCharMatrix(m, align="left")
  )
})

test_that("align accepts a per-column vector", {

  m <- matrix(c("x", "yyy", "zz", "w"), nrow = 1)  # 4 columns, 1 row

  out <- capture.output(
    printCharMatrix(m, align = c("left", "right", "left", "right"),
                    sep = 1, showRownames = FALSE)
  )
  # left col: value at start of its field; right col: value at the end
  expect_match(out[1], "^x")          # left-aligned first column
  expect_match(out[1], "w$")          # right-aligned last column
})

test_that("align of length 1 is recycled across all columns", {

  m <- matrix(c("a", "b", "c"), nrow = 1)  # 3 columns

  expect_invisible(printCharMatrix(m, align = "left"))
  expect_invisible(printCharMatrix(m, align = "right"))
})

test_that("align supports partial matching", {

  m <- matrix(c("a", "b"), nrow = 1)

  expect_invisible(printCharMatrix(m, align = "l"))   # -> "left"
  expect_invisible(printCharMatrix(m, align = "r"))   # -> "right"
})

test_that("wrong align length errors", {

  m <- matrix(c("a", "b", "c"), nrow = 1)  # 3 columns

  expect_error(
    printCharMatrix(m, align = c("left", "right")),
    "length 1 or equal to the number of columns"
  )
})

test_that("invalid align value errors", {

  m <- matrix(c("a", "b"), nrow = 1)

  expect_error(printCharMatrix(m, align = "center"))
})

test_that("no leading blank line for unnamed matrix", {

  m <- matrix(letters[1:6], nrow = 3)  # no dimnames

  out <- capture.output(printCharMatrix(m))
  expect_false(grepl("^[[:space:]]*$", out[1]))  # first line is not blank
})

test_that("no trailing blank line for a single-block matrix", {

  m <- matrix(letters[1:6], nrow = 3)

  out <- capture.output(printCharMatrix(m))
  expect_false(grepl("^[[:space:]]*$", out[length(out)]))  # last line not blank
})

test_that("column names are printed when present", {

  m <- matrix(c("a", "b", "c", "d"), nrow = 2,
              dimnames = list(NULL, c("Foo", "Bar")))

  out <- capture.output(printCharMatrix(m, showRownames = FALSE))
  expect_match(out[1], "Foo")
  expect_match(out[1], "Bar")
})

test_that("wide matrix is split into blocks separated by a blank line", {

  m <- matrix(letters[1:15], nrow = 3)  # 5 columns

  out <- capture.output(printCharMatrix(m, width = 10))
  # more than one block -> at least one interior blank separator line
  expect_true(any(grepl("^[[:space:]]*$", out)))
})

test_that("fully-NA column prints 'NA' instead of failing", {

  expect_output(printCharMatrix(matrix(NA_character_, 2L, 1L)), "NA")
})

test_that("single named column prints its header", {

  expect_output(
    printCharMatrix(matrix("x", dimnames = list(NULL, "A"))),
    "^A"
  )
})

test_that("empty matrix is handled silently", {

  expect_silent(
    printCharMatrix(matrix(character(), nrow = 0L, ncol = 2L))
  )
})

test_that("invalid sep errors", {
  expect_error(printCharMatrix(matrix("x"), sep = -1))
  expect_error(printCharMatrix(matrix("x"), sep = 1.5))
  expect_error(printCharMatrix(matrix("x"), sep = "x"))
  expect_error(printCharMatrix(matrix("x"), sep = Inf))
})

test_that("invalid width errors", {
  expect_error(printCharMatrix(matrix("x"), width = NA))
  expect_error(printCharMatrix(matrix("x"), width = "80"))
  expect_error(printCharMatrix(matrix("x"), width = TRUE))
  expect_error(printCharMatrix(matrix("x"), width = Inf))
})

test_that("NA in dimnames is shown as 'NA'", {
  m <- matrix(c("a", "b"), nrow = 1,
              dimnames = list(NA_character_, c("A", NA_character_)))
  out <- capture.output(printCharMatrix(m))
  expect_match(out[1], "NA")   # NA colname rendered
})

test_that("invalid logical flags error", {
  expect_error(printCharMatrix(matrix("x"), showRownames = NA))
})

test_that("mismatched align length still errors on a 1-col matrix", {
  expect_error(printCharMatrix(matrix("x"), align = c("left", "right")))
})
