library(testthat)

test_that("label gets and sets atomic labels", {

  x <- 1:5
  label(x) <- "numbers"

  expect_equal(label(x), "numbers")
})

test_that("label gets and sets data frame labels", {

  df <- data.frame(a=1:3)

  label(df) <- "dataset"

  expect_equal(label(df), "dataset")
})

test_that("label sets variable labels", {

  df <- data.frame(a=1:3, b=4:6)

  label(df, vars=TRUE) <- c("A","B")

  expect_equal(
    label(df, vars=TRUE),
    c(a="A", b="B")
  )
})

test_that("label replicates scalar variable label", {

  df <- data.frame(a=1:3, b=4:6)

  label(df, vars=TRUE) <- "X"

  expect_equal(
    unname(label(df, vars=TRUE)),
    c("X","X")
  )
})



test_that("label returns NA for unlabelled variables", {
  df <- data.frame(a = 1:3, b = 4:6)
  label(df$a) <- "A"
  
  expect_identical(label(df, vars = TRUE), c(a = "A", b = NA_character_))
  expect_null(label(df))
})

test_that("label accepts names and numeric column indices", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  
  label(df, vars = c("a", "c")) <- c("A", "C")
  expect_identical(label(df, vars = c("c", "a")), c(c = "C", a = "A"))
  
  label(df, vars = c(1, 2)) <- c("First", "Second")
  expect_identical(
    label(df, vars = c(1, 2)),
    c(a = "First", b = "Second")
  )
})

test_that("label removes object and variable labels with NULL", {
  df <- data.frame(a = 1:3, b = 4:6)
  label(df) <- "Dataset"
  label(df, vars = TRUE) <- c("A", "B")
  
  label(df) <- NULL
  expect_null(label(df))
  
  label(df, vars = TRUE) <- NULL
  expect_identical(
    label(df, vars = TRUE),
    c(a = NA_character_, b = NA_character_)
  )
})

test_that("label validates vars and replacement values", {
  df <- data.frame(a = 1:3, b = 4:6)
  
  expect_error(label(df, vars = "missing"), "Unknown variable")
  expect_error(label(df, vars = 0), "out of range")
  expect_error(label(df, vars = 3), "out of range")
  expect_error(label(df, vars = FALSE), "must be TRUE")
  
  expect_error(label(df, vars = TRUE) <- c("A", "B", "C"),
               "same length as vars")
  expect_error(label(df) <- c("A", "B"), "length 1")
  expect_error(label(df) <- list("A"), "cannot assign a list")
})


