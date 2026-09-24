
# =========================================================
# test-strX.R
# =========================================================

# strX() prints as a side effect; the output is captured in every test so
# that it does not leak onto the test console

test_that("strX returns invisible character vector", {
  
  # the test name claimed invisibility, but the value was never checked
  out <- capture.output(res <- withVisible(strX(mtcars)))
  
  expect_false(res$visible)
  expect_type(res$value, "character")
  expect_gt(length(out), 0L)
  
})

test_that("strX numbers variables", {
  
  out <- capture.output(
    strX(mtcars)
  )
  
  expect_true(
    any(grepl("1 \\$", out))
  )
  
})

test_that("strX works without numbering", {
  
  out <- capture.output(
    strX(
      mtcars,
      enumerate = FALSE
    )
  )
  
  expect_false(
    any(grepl("1 \\$", out))
  )
  
})

# nested list used below: x[[2]] is a list of two, x[[2]][[2]] a list of one
.strxNested <- list(a = 1, b = list(c = 2, d = list(e = 3)), f = "z")

test_that("without recursive only top-level elements are numbered", {
  
  out <- capture.output(strX(.strxNested))
  
  expect_true(any(grepl("^ 1 \\$ a:", out)))
  expect_true(any(grepl("^ 3 \\$ f:", out)))
  expect_true(any(grepl("^  \\.\\.\\$ c:", out)))       # nested: unnumbered
  
})

test_that("recursive numbering is hierarchical and positional", {
  
  out <- capture.output(strX(.strxNested, recursive = TRUE))
  
  expect_true(any(grepl("\\.\\.2\\.1 \\$ c:", out)))
  expect_true(any(grepl("\\.\\.2\\.2 \\$ d:", out)))
  expect_true(any(grepl("\\.\\. \\.\\.2\\.2\\.1 \\$ e:", out)))
  # top-level numbers do not depend on recursive: f stays 3 (it was 6 with
  # a running number across all levels)
  expect_true(any(grepl("^ 3 \\$ f:", out)))
  
})

test_that("top-level numbers are right-aligned", {
  
  out <- capture.output(strX(mtcars))
  
  expect_true(any(grepl("^  1 \\$ mpg", out)))
  expect_true(any(grepl("^ 11 \\$ carb", out)))
  
})

test_that("the returned lines are the printed lines", {
  
  out <- capture.output(res <- strX(.strxNested, recursive = TRUE))
  
  expect_identical(res, out)
  
})

test_that("non-list objects are printed as by str()", {
  
  out <- capture.output(res <- strX(1:3))
  
  expect_identical(out, capture.output(str(1:3)))
  
})

test_that("strX errors on invalid logical arguments", {
  
  expect_error(strX(mtcars, recursive = 1:2), "'recursive' must be TRUE or FALSE")
  expect_error(strX(mtcars, recursive = "yes"), "'recursive'")
  # NA passed the old stopifnot() and failed later inside if()
  expect_error(strX(mtcars, enumerate = NA), "'enumerate' must be TRUE or FALSE")
  
})

