library(testthat)

test_that("splitPath extracts filename", {

  p <- splitPath("test/file.txt")

  expect_equal(p$filename, "file")
  expect_equal(p$extension, "txt")
})

test_that("splitPath handles directories", {

  p <- splitPath("test/folder/", last.is.file=FALSE)

  expect_true(is.na(p$filename))
})

test_that("splitPath returns list", {

  p <- splitPath("abc.txt")

  expect_true(is.list(p))
})
