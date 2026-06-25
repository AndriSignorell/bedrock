
# ------------------------------------------------------------------------------
# .resolveCourseURL (internal) — tested via courseData
# ------------------------------------------------------------------------------

test_that(".resolveCourseURL returns first valid URL", {
  mockery::stub(.resolveCourseURL, "fileExistURL", function(url, ...) {
    grepl("hwz", url)
  })
  res <- .resolveCourseURL("data.csv",
                           c("http://www.signorell.net/hwz/datasets/",
                             "http://www.signorell.net/buch/"))
  expect_equal(res, "http://www.signorell.net/hwz/datasets/")
})

test_that(".resolveCourseURL returns NULL when nothing found", {
  mockery::stub(.resolveCourseURL, "fileExistURL", function(...) FALSE)
  res <- .resolveCourseURL("data.csv",
                           c("http://a.example/", "http://b.example/"))
  expect_null(res)
})


# ------------------------------------------------------------------------------
# courseData
# ------------------------------------------------------------------------------

test_that("courseData errors when file not found in any candidate", {
  mockery::stub(courseData, "fileExistURL", function(...) FALSE)
  expect_error(courseData("ghost.csv"), "ghost.csv")
})

test_that("courseData errors when explicit url does not contain file", {
  mockery::stub(courseData, "fileExistURL", function(...) FALSE)
  expect_error(courseData("ghost.csv", url = "http://example.com/"),
               "existiert nicht")
})

test_that("courseData dispatches to read.table for .csv", {
  mockery::stub(courseData, "fileExistURL",  function(...) TRUE)
  mockery::stub(courseData, "read.table", function(path, ...) {
    data.frame(x = 1:2)
  })
  res <- courseData("data.csv", url = "http://example.com/")
  expect_s3_class(res, "data.frame")
})

test_that("courseData dispatches to openDataObject for .xlsx", {
  mockery::stub(courseData, "fileExistURL",   function(...) TRUE)
  mockery::stub(courseData, "openDataObject", function(...) data.frame(x = 1))
  res <- courseData("data.xlsx", url = "http://example.com/")
  expect_s3_class(res, "data.frame")
})


