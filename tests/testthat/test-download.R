
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


test_that(".getDownloadsPath returns a non-empty scalar path", {
  out <- .getDownloadsPath()
  
  expect_type(out, "character")
  expect_length(out, 1L)
  expect_true(nzchar(out))
})

test_that(".getDownloadsPath reads the Windows Downloads registry entry", {
  if (.Platform$OS.type == "windows") {
    guid <- "{374DE290-123F-4565-9164-39C4925E467B}"
    registry <- setNames(list("C:\\Users\\tester\\Downloads"), guid)
    
    local_mocked_bindings(
      readRegistry = function(...) registry,
      .package = "utils"
    )
    
    expect_identical(
      .getDownloadsPath(),
      normalizePath(registry[[guid]], winslash = "/", mustWork = FALSE)
    )
  } else {
    expect_true(TRUE)
  }
})

test_that(".getDownloadsPath falls back when the Windows registry fails", {
  if (.Platform$OS.type == "windows") {
    local_mocked_bindings(
      readRegistry = function(...) stop("registry unavailable"),
      .package = "utils"
    )
    
    expect_identical(
      .getDownloadsPath(),
      file.path(path.expand("~"), "Downloads")
    )
  } else {
    expect_true(TRUE)
  }
})

test_that(".getDownloadsPath expands the Linux XDG home variable", {
  if (Sys.info()[["sysname"]] == "Linux") {
    home <- tempfile("bedrock-home-")
    configDir <- file.path(home, ".config")
    dir.create(configDir, recursive = TRUE)
    on.exit(unlink(home, recursive = TRUE), add = TRUE)
    
    writeLines(
      'XDG_DOWNLOAD_DIR="$HOME/My Downloads"',
      file.path(configDir, "user-dirs.dirs")
    )
    
    local_mocked_bindings(
      path.expand = function(path) {
        if (identical(path, "~")) home else path
      },
      .package = "base"
    )
    
    expect_identical(
      .getDownloadsPath(),
      normalizePath(
        file.path(home, "My Downloads"),
        winslash = "/",
        mustWork = FALSE
      )
    )
  } else {
    expect_true(TRUE)
  }
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


test_that("readDownload reads CSV and TSV files", {
  skip_if_not_installed("readr")
  
  csv <- tempfile(fileext = ".csv")
  tsv <- tempfile(fileext = ".tsv")
  on.exit(unlink(c(csv, tsv)), add = TRUE)
  writeLines(c("id,value", "1,a", "2,b"), csv)
  writeLines(c("id\tvalue", "1\ta", "2\tb"), tsv)
  
  current <- csv
  local_mocked_bindings(findDownload = function(...) current)
  
  out_csv <- readDownload("data.csv")
  current <- tsv
  out_tsv <- readDownload("data.tsv")
  
  expect_s3_class(out_csv, "data.frame")
  expect_false(inherits(out_csv, "tbl_df"))
  expect_identical(out_csv$id, c(1, 2))
  expect_identical(out_tsv, out_csv)
})

test_that("readDownload reads delimited text and can retain a tibble", {
  skip_if_not_installed("readr")
  
  tf <- tempfile(fileext = ".txt")
  on.exit(unlink(tf), add = TRUE)
  writeLines(c("id|value", "1|a", "2|b"), tf)
  
  local_mocked_bindings(findDownload = function(...) tf)
  
  out <- readDownload("data.txt", delim = "|", output = "tibble")
  
  expect_s3_class(out, "tbl_df")
  expect_identical(out$id, c(1, 2))
})

test_that("readDownload validates the extension and output", {
  local_mocked_bindings(findDownload = function(...) "/tmp/file")
  expect_error(readDownload("file"), "has no extension")
  
  expect_error(readDownload("file.csv", output = "matrix"), "arg")
})

