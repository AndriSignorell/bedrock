
# ── isURL / isFilePath ────────────────────────────────────────────────────────

test_that("isURL returns TRUE for http/https", {
  expect_true(isURL("https://example.com/data.csv"))
  expect_true(isURL("http://example.com/data.csv"))
})

test_that("isURL returns TRUE for ftp and cloud schemes", {
  expect_true(isURL("ftp://files.example.org/x.zip"))
  expect_true(isURL("s3://my-bucket/file.parquet"))
})

test_that("isURL returns FALSE for file paths", {
  expect_false(isURL("/home/user/file.csv"))
  expect_false(isURL("./script.R"))
  expect_false(isURL("C:/Users/Hans/file.xlsx"))
})

test_that("isFilePath returns TRUE for local paths", {
  expect_true(isFilePath("/home/user/data/file.csv"))
  expect_true(isFilePath("~/documents/report.pdf"))
  expect_true(isFilePath("./relative/path/file.R"))
  expect_true(isFilePath("../other/folder/data.rds"))
  expect_true(isFilePath("C:/Users/Hans/file.xlsx"))
})

test_that("isFilePath returns FALSE for URLs", {
  expect_false(isFilePath("https://example.com/f.csv"))
})



test_that("isURL recognises all documented schemes case-insensitively", {
  urls <- c(
    "HTTPS://example.com/data.csv",
    "ftps://files.example.org/data.csv",
    "file:///tmp/data.csv",
    "gs://bucket/data.csv",
    "az://container/data.csv"
  )
  
  expect_true(all(vapply(urls, isURL, logical(1L))))
  expect_false(any(vapply(urls, isFilePath, logical(1L))))
})

test_that("isFilePath recognises ambiguous and backslash paths", {
  paths <- c(
    "folder/data.csv",
    "folder\\data.csv",
    "\\\\server\\share\\data.csv"
  )
  
  expect_true(all(vapply(paths, isFilePath, logical(1L))))
  expect_false(any(vapply(paths, isURL, logical(1L))))
})

test_that("URL detection distinguishes unknown strings", {
  expect_identical(.detectInputType("report.csv"), "unknown")
  expect_identical(.detectInputType(""), "unknown")
  expect_false(isURL("mailto:user@example.com"))
  expect_false(isFilePath("mailto:user@example.com"))
})

test_that("URL and path detection validate scalar character input", {
  expect_error(isURL(1), "single character string")
  expect_error(isURL(character()), "single character string")
  expect_error(isFilePath(c("a", "b")), "single character string")
})

