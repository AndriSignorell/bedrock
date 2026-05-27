# ── Tests for stringsAsFactors() ────────────────────────────────────────────

library(testthat)

# shared test fixture
d <- data.frame(
  char_x = LETTERS[1:5],
  char_y = LETTERS[6:10],
  num_z  = 1:5,
  stringsAsFactors = FALSE
)

# ── NULL: convert all character columns ─────────────────────────────────────

test_that("NULL converts all character columns", {
  out <- stringsAsFactors(d)
  expect_s3_class(out$char_x, "factor")
  expect_s3_class(out$char_y, "factor")
  expect_type(out$num_z, "integer")        # numeric untouched
})

# ── positive column names ────────────────────────────────────────────────────

test_that("single column name converts only that column", {
  out <- stringsAsFactors(d, columns = "char_y")
  expect_type(out$char_x, "character")     # untouched
  expect_s3_class(out$char_y, "factor")
  expect_type(out$num_z, "integer")
})

test_that("multiple column names convert exactly those columns", {
  out <- stringsAsFactors(d, columns = c("char_x", "char_y"))
  expect_s3_class(out$char_x, "factor")
  expect_s3_class(out$char_y, "factor")
  expect_type(out$num_z, "integer")
})

# ── positive numeric indices ─────────────────────────────────────────────────

test_that("positive index converts that character column", {
  out <- stringsAsFactors(d, columns = 1)
  expect_s3_class(out$char_x, "factor")
  expect_type(out$char_y, "character")
})

test_that("positive index pointing to numeric column is silently skipped", {
  out <- stringsAsFactors(d, columns = 3)
  expect_type(out$num_z, "integer")        # must NOT become factor
  expect_type(out$char_x, "character")
  expect_type(out$char_y, "character")
})

# ── negative numeric indices ─────────────────────────────────────────────────

test_that("negative index excludes that column, converts remaining chars", {
  out <- stringsAsFactors(d, columns = -2)  # exclude char_y
  expect_s3_class(out$char_x, "factor")
  expect_type(out$char_y, "character")      # excluded
  expect_type(out$num_z, "integer")
})

test_that("negative index pointing to numeric column leaves chars untouched", {
  out <- stringsAsFactors(d, columns = -3)  # exclude num_z
  expect_s3_class(out$char_x, "factor")
  expect_s3_class(out$char_y, "factor")
  expect_type(out$num_z, "integer")
})

# ── return value ─────────────────────────────────────────────────────────────

test_that("input data.frame is returned (not NULL or different object)", {
  out <- stringsAsFactors(d)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), nrow(d))
  expect_equal(ncol(out), ncol(d))
  expect_equal(names(out), names(d))
})

test_that("original data.frame is not modified in place", {
  original <- d
  stringsAsFactors(d)
  expect_equal(d, original)
})

# ── error cases ──────────────────────────────────────────────────────────────

test_that("unknown column name throws an error", {
  expect_error(
    stringsAsFactors(d, columns = "does_not_exist"),
    regexp = "not found"
  )
})

test_that("mixed positive and negative indices throw an error", {
  expect_error(
    stringsAsFactors(d, columns = c(-1, 2)),
    regexp = "all positive or all negative"
  )
})

test_that("invalid columns type throws an error", {
  expect_error(
    stringsAsFactors(d, columns = TRUE),
    regexp = "NULL, a numeric vector, or a character vector"
  )
})

