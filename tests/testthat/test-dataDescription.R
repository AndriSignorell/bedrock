# ------------------------------------------------------------------------------
# dataDescription
# ------------------------------------------------------------------------------

test_that("dataDescription returns NULL when sheet missing", {
  mockery::stub(dataDescription, "readxl::excel_sheets",
                function(...) "Sheet1")
  res <- dataDescription("dummy.xlsx", sheet = "Description")
  expect_null(res)
})

test_that("dataDescription returns NULL when only one sheet", {
  mockery::stub(dataDescription, "readxl::excel_sheets",
                function(...) "Description")
  res <- dataDescription("dummy.xlsx", sheet = "Description")
  expect_null(res)
})

test_that("dataDescription parses codes correctly", {
  mockery::stub(dataDescription, "readxl::excel_sheets",
                function(...) c("Data", "Description"))
  mockery::stub(dataDescription, "readxl::read_excel", function(fn, sheet, ...) {
    data.frame(
      Variable    = c("sex",         "age"),
      Beschreibung = c("Gender",      "Age"),
      Codes        = c("1=Male\r\n2=Female", NA),
      stringsAsFactors = FALSE
    )
  })
  res <- dataDescription("dummy.xlsx")
  expect_named(res, c("desctable", "codes"))
  expect_equal(res$codes[["sex"]], c("1=Male", "2=Female"))
  expect_length(res$codes, 1L)   # age has no codes
})

test_that("dataDescription trims trailing empty rows", {
  mockery::stub(dataDescription, "readxl::excel_sheets",
                function(...) c("Data", "Description"))
  mockery::stub(dataDescription, "readxl::read_excel", function(...) {
    data.frame(
      Variable     = c("sex", NA),
      Beschreibung = c("Gender", NA),
      Codes        = c(NA, NA),
      stringsAsFactors = FALSE
    )
  })
  res <- dataDescription("dummy.xlsx")
  expect_equal(nrow(res$desctable), 1L)
})


