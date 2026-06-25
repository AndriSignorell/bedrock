
# ------------------------------------------------------------------------------
# Hilfsfunktion
# ------------------------------------------------------------------------------

.fixture <- function(name) {
  system.file("tests", "testthat", "testdata", name, package = "bedrock",
              mustWork = FALSE) |>
    (\(p) if (nzchar(p)) p else
      file.path("testdata", name))()   # fallback beim direkten testthat-Aufruf
}

# ------------------------------------------------------------------------------
# findDownload
# ------------------------------------------------------------------------------

test_that("findDownload returns full path for existing file", {
  tf <- .fixture("fixture.xlsx")
  skip_if(!file.exists(tf), "fixture nicht gefunden")
  
  mockery::stub(findDownload, ".getDownloadsPath",
                function() dirname(tf))
  res <- findDownload(basename(tf))
  expect_equal(res, tf)
})

test_that("findDownload errors for missing file", {
  mockery::stub(findDownload, ".getDownloadsPath",
                function() tempdir())
  expect_error(findDownload("definitely_not_there_xyz.xlsx"),
               "File not found")
})

# ------------------------------------------------------------------------------
# readDownload
# ------------------------------------------------------------------------------

test_that("readDownload reads XLSX", {
  tf <- .fixture("fixture.xlsx")
  skip_if(!file.exists(tf), "fixture nicht gefunden")
  
  mockery::stub(readDownload, "findDownload", function(...) tf)
  mockery::stub(readDownload, "toBaseR", as.data.frame)
  res <- readDownload("dummy.xlsx")
  expect_s3_class(res, "data.frame")
  expect_true("a" %in% names(res))
})

test_that("readDownload base = FALSE skips toBaseR", {
  tf <- .fixture("fixture.xlsx")
  skip_if(!file.exists(tf), "fixture nicht gefunden")
  
  mockery::stub(readDownload, "findDownload", function(...) tf)
  # wenn toBaseR aufgerufen würde, würde dieser stub einen Fehler werfen
  mockery::stub(readDownload, "toBaseR",
                function(...) stop("toBaseR should not be called"))
  expect_no_error(readDownload("dummy.xlsx", base = FALSE))
})


test_that("readDownload errors on unsupported extension", {
  mockery::stub(readDownload, "findDownload",
                function(...) "/tmp/file.parquet")
  expect_error(readDownload("file.parquet"), "Unsupported file type")
})

