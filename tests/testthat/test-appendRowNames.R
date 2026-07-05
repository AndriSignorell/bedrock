
library(testthat)

test_that("appendRowNames appends row names", {

  x <- data.frame(
    a = 1:3,
    row.names = letters[1:3]
  )

  res <- appendRowNames(x)

  expect_true(
    "rowname" %in% names(res)
  )

  expect_equal(
    res$rowname,
    letters[1:3]
  )

})


test_that("appendRowNames removes row names by default", {

  x <- data.frame(
    a = 1:3,
    row.names = letters[1:3]
  )

  res <- appendRowNames(x)

  # data.frames never have NULL row names; removing them yields
  # automatic (compact) row names, indicated by a negative .row_names_info
  expect_true(.row_names_info(res) < 0L)

  expect_identical(
    rownames(res),
    as.character(1:3)
  )

})


test_that("appendRowNames keeps row names if requested", {

  x <- data.frame(
    a = 1:3,
    row.names = letters[1:3]
  )

  res <- appendRowNames(
    x,
    removeRowNames = FALSE
  )

  expect_true(
    "rowname" %in% names(res)
  )

  expect_equal(
    rownames(res),
    letters[1:3]
  )

})


test_that("appendRowNames works for matrices", {

  x <- matrix(1:4, nrow = 2)

  rownames(x) <- c("r1", "r2")

  res <- appendRowNames(x)

  expect_true(is.matrix(res))

  expect_equal(
    res[, 1],
    c("r1", "r2"),
    ignore_attr = TRUE
  )

})


test_that("appendRowNames uses default row indices", {

  x <- data.frame(a = 1:3)

  rownames(x) <- NULL

  res <- appendRowNames(x)

  # rownames() on a data.frame always returns character,
  # even for automatic row names
  expect_equal(
    res$rowname,
    c("1", "2", "3")
  )

})


test_that("appendRowNames errors for invalid input", {

  expect_error(
    appendRowNames(1:5),
    "data.frame or matrix"
  )

})
