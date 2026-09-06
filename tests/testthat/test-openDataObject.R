

test_that("openDataObject returns unlabelled data when doc is NA", {
  dat <- data.frame(id = 1:3, value = c("a", "b", "c"))
  
  local_mocked_bindings(
    GET = function(...) structure(list(), class = "mock_response"),
    write_disk = function(...) NULL,
    http_status = function(...) list(category = "Success"),
    status_code = function(...) 200L,
    .package = "httr"
  )
  local_mocked_bindings(
    read_excel = function(path, sheet = NULL, ...) dat,
    excel_sheets = function(path) "Data",
    .package = "readxl"
  )
  
  out <- openDataObject("example.xlsx", url = "https://example.test", doc = NA)
  
  expect_s3_class(out, "data.frame")
  expect_identical(out, dat)
})

test_that("openDataObject applies codes, factor order and labels", {
  dat <- data.frame(
    sex = c(1, 2, 3),
    grade = c(1, 2, 3),
    value = c(10, 20, 30)
  )
  code <- data.frame(
    Variable = c("sex", "grade", "value"),
    Beschreibung = c("Sex", "Grade", "Measured value"),
    Codes = c(
      "1=Male\r\n2=Female",
      "1=Low\r\n2=Medium\r\n3=High",
      NA_character_
    ),
    Skala = c("nominal", "ordinal", "metric")
  )
  
  local_mocked_bindings(
    GET = function(...) structure(list(), class = "mock_response"),
    write_disk = function(...) NULL,
    http_status = function(...) list(category = "Success"),
    status_code = function(...) 200L,
    .package = "httr"
  )
  local_mocked_bindings(
    read_excel = function(path, sheet = NULL, ...) {
      if (is.null(sheet)) dat else code
    },
    excel_sheets = function(path) c("Data", "Description"),
    .package = "readxl"
  )
  
  out <- openDataObject("example.xlsx", url = "https://example.test")
  
  expect_true(is.factor(out$sex))
  expect_false(is.ordered(out$sex))
  expect_identical(levels(out$sex), c("Male", "Female"))
  expect_identical(as.character(out$sex), c("Male", "Female", NA_character_))
  
  expect_true(is.ordered(out$grade))
  expect_identical(levels(out$grade), c("Low", "Medium", "High"))
  expect_identical(as.character(out$grade), c("Low", "Medium", "High"))
  
  expect_identical(label(out$sex), "Sex")
  expect_identical(label(out$grade), "Grade")
  expect_identical(label(out$value), "Measured value")
})

test_that("openDataObject only auto-detects a Description sheet", {
  dat <- data.frame(id = 1:2, value = c("a", "b"))
  
  local_mocked_bindings(
    GET = function(...) structure(list(), class = "mock_response"),
    write_disk = function(...) NULL,
    http_status = function(...) list(category = "Success"),
    status_code = function(...) 200L,
    .package = "httr"
  )
  local_mocked_bindings(
    read_excel = function(path, sheet = NULL, ...) {
      if (!is.null(sheet))
        stop("metadata sheet must not be read")
      dat
    },
    excel_sheets = function(path) c("Data", "Notes"),
    .package = "readxl"
  )
  
  out <- openDataObject("example.xlsx", url = "https://example.test")
  
  expect_identical(out, dat)
})

test_that("openDataObject reports HTTP failures", {
  local_mocked_bindings(
    GET = function(...) structure(list(), class = "mock_response"),
    write_disk = function(...) NULL,
    http_status = function(...) list(category = "Client error"),
    status_code = function(...) 404L,
    .package = "httr"
  )
  
  expect_error(
    openDataObject("missing.xlsx", url = "https://example.test", doc = NA),
    "Download failed \\[404\\]"
  )
})
