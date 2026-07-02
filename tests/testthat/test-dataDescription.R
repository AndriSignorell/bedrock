# ------------------------------------------------------------------------------
# dataDescription
# ------------------------------------------------------------------------------
# Voraussetzung: dataDescription() ruft excel_sheets()/read_excel() OHNE
# readxl::-Prefix auf (@importFrom readxl excel_sheets read_excel), sonst
# greifen die Mocks nicht.

test_that("dataDescription returns NULL when sheet missing", {
  local_mocked_bindings(
    excel_sheets = function(...) "Sheet1"
  )
  res <- dataDescription("dummy.xlsx", sheet = "Description")
  expect_null(res)
})

test_that("dataDescription returns NULL when only one sheet", {
  local_mocked_bindings(
    excel_sheets = function(...) "Description"
  )
  res <- dataDescription("dummy.xlsx", sheet = "Description")
  expect_null(res)
})

test_that("dataDescription parses codes correctly", {
  local_mocked_bindings(
    excel_sheets = function(...) c("Data", "Description"),
    read_excel   = function(fn, sheet, ...) {
      data.frame(
        Variable     = c("sex",                "age"),
        Beschreibung = c("Gender",             "Age"),
        Codes        = c("1=Male\r\n2=Female", NA),
        stringsAsFactors = FALSE
      )
    }
  )
  res <- dataDescription("dummy.xlsx")
  expect_named(res, c("desctable", "codes"))
  expect_equal(res$codes[["sex"]], c("1=Male", "2=Female"))
  expect_length(res$codes, 1L)   # age has no codes
})

test_that("dataDescription trims trailing empty rows", {
  local_mocked_bindings(
    excel_sheets = function(...) c("Data", "Description"),
    read_excel   = function(...) {
      data.frame(
        Variable     = c("sex", NA),
        Beschreibung = c("Gender", NA),
        Codes        = c(NA, NA),
        stringsAsFactors = FALSE
      )
    }
  )
  res <- dataDescription("dummy.xlsx")
  expect_equal(nrow(res$desctable), 1L)
})
