
# ------------------------------------------------------------------------------
# .resolveCourseURL (internal) — tested via readCourseData
# ------------------------------------------------------------------------------

test_that(".resolveCourseURL returns first valid URL", {
  local_mocked_bindings(
    urlExists = function(url, ...) grepl("hwz", url)
  )
  res <- .resolveCourseURL("data.csv",
                           c("http://www.signorell.net/hwz/datasets/",
                             "http://www.signorell.net/buch/"))
  expect_equal(res, "http://www.signorell.net/hwz/datasets/")
})

test_that(".resolveCourseURL returns NULL when nothing found", {
  local_mocked_bindings(
    urlExists = function(...) FALSE
  )
  res <- .resolveCourseURL("data.csv",
                           c("http://a.example/", "http://b.example/"))
  expect_null(res)
})


# ------------------------------------------------------------------------------
# readCourseData
# ------------------------------------------------------------------------------

test_that("readCourseData errors when file not found in any candidate", {
  local_mocked_bindings(
    urlExists = function(...) FALSE
  )
  expect_error(readCourseData("ghost.csv"), "ghost.csv")
})

test_that("readCourseData errors when explicit url does not contain file", {
  local_mocked_bindings(
    urlExists = function(...) FALSE
  )
  expect_error(readCourseData("ghost.csv", url = "http://example.com/"),
               "does not exist")
})

test_that("readCourseData dispatches to read.table for .csv", {
  local_mocked_bindings(
    urlExists = function(...) TRUE,
    read.table   = function(path, ...) data.frame(x = 1:2)
  )
  res <- readCourseData("data.csv", url = "http://example.com/")
  expect_s3_class(res, "data.frame")
})

test_that("readCourseData dispatches to openDataObject for .xlsx", {
  local_mocked_bindings(
    urlExists   = function(...) TRUE,
    openDataObject = function(...) data.frame(x = 1)
  )
  res <- readCourseData("data.xlsx", url = "http://example.com/")
  expect_s3_class(res, "data.frame")
})
