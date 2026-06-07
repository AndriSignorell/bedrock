
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


