
# ------------------------------------------------------------------------------
# findDownload
# ------------------------------------------------------------------------------

test_that("findDownload returns full path for existing file", {
  tf <- test_path("testdata", "fixture.xlsx")
  skip_if_not(file.exists(tf), "fixture nicht gefunden")

  local_mocked_bindings(
    .getDownloadsPath = function() dirname(tf)
  )
  res <- findDownload(basename(tf))
  expect_equal(res, tf)
})

test_that("findDownload errors for missing file", {
  local_mocked_bindings(
    .getDownloadsPath = function() tempdir()
  )
  expect_error(findDownload("definitely_not_there_xyz.xlsx"),
               "File not found")
})

# ------------------------------------------------------------------------------
# readDownload
# ------------------------------------------------------------------------------

test_that("readDownload reads XLSX", {
  tf <- test_path("testdata", "fixture.xlsx")
  skip_if_not(file.exists(tf), "fixture nicht gefunden")

  local_mocked_bindings(
    findDownload = function(...) tf,
    toBaseR      = as.data.frame
  )
  res <- readDownload("dummy.xlsx")
  expect_s3_class(res, "data.frame")
  expect_true("a" %in% names(res))
})

test_that("readDownload output = 'tibble' skips toBaseR", {
  tf <- test_path("testdata", "fixture.xlsx")
  skip_if_not(file.exists(tf), "fixture nicht gefunden")

  local_mocked_bindings(
    findDownload = function(...) tf,
    # wenn toBaseR aufgerufen würde, würde dieser Mock einen Fehler werfen
    toBaseR      = function(...) stop("toBaseR should not be called")
  )
  res <- readDownload("dummy.xlsx", output = "tibble")
  expect_s3_class(res, "tbl_df")
})

test_that("readDownload errors on unsupported extension", {
  local_mocked_bindings(
    findDownload = function(...) "/tmp/file.parquet"
  )
  expect_error(readDownload("file.parquet"), "unsupported file type")
})
