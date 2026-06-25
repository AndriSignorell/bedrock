
library(testthat)

test_that("performs simple substitutions", {
  expect_equal(
    mGsub(
      c("foo bar", "bar foo"),
      c("foo", "bar"),
      c("FOO", "BAR")
    ),
    c("FOO BAR", "BAR FOO")
  )
})

test_that("avoids cascade replacements", {
  expect_equal(
    mGsub(
      c("foo bar", "bar foo"),
      c("foo", "bar"),
      c("bar", "foo")
    ),
    c("bar foo", "foo bar")
  )
})

test_that("replaces all occurrences within strings", {
  expect_equal(
    mGsub(
      c("aaa", "aba"),
      c("a"),
      c("X")
    ),
    c("XXX", "XbX")
  )
})

test_that("handles multiple patterns", {
  expect_equal(
    mGsub(
      c("A B C"),
      c("A", "B", "C"),
      c("X", "Y", "Z")
    ),
    "X Y Z"
  )
})

test_that("works with overlapping replacements", {
  expect_equal(
    mGsub(
      c("A", "B", "AB", "BA"),
      c("A", "B"),
      c("BX", "CY")
    ),
    c("BX", "CY", "BXCY", "CYBX")
  )
})

test_that("non-matching strings remain unchanged", {
  x <- c("foo", "bar")
  
  expect_equal(
    mGsub(x, c("xyz"), c("XYZ")),
    x
  )
})

test_that("works with empty input vector", {
  expect_equal(
    mGsub(
      character(0),
      c("a"),
      c("A")
    ),
    character(0)
  )
})

test_that("works with empty pattern set", {
  x <- c("a", "b", "c")
  
  expect_equal(
    mGsub(
      x,
      character(0),
      character(0)
    ),
    x
  )
})

test_that("handles NA values", {
  expect_equal(
    mGsub(
      c("foo", NA, "bar"),
      c("foo", "bar"),
      c("FOO", "BAR")
    ),
    c("FOO", NA, "BAR")
  )
})

test_that("preserves vector length", {
  x <- c("foo", "bar", "baz")
  
  expect_length(
    mGsub(
      x,
      c("foo"),
      c("FOO")
    ),
    length(x)
  )
})

test_that("returns character vector", {
  expect_type(
    mGsub(
      c("foo"),
      c("foo"),
      c("FOO")
    ),
    "character"
  )
})

test_that("throws error when patterns and replacements differ in length", {
  expect_error(
    mGsub(
      c("foo"),
      c("foo", "bar"),
      c("FOO")
    ),
    class = "simpleError"
  )
})

test_that("uses fixed matching and not regex", {
  expect_equal(
    mGsub(
      c("a.b"),
      c("."),
      c("-")
    ),
    "a-b"
  )
})

test_that("replacement strings may contain pattern strings", {
  expect_equal(
    mGsub(
      c("A B"),
      c("A", "B"),
      c("BB", "AA")
    ),
    "BB AA"
  )
})

