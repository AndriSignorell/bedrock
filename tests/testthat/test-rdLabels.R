# ------------------------------------------------------------------------------
# rdLabels
# ------------------------------------------------------------------------------
# Requires Rd_db() to be imported via @importFrom tools and called in
# rdLabels() WITHOUT the tools:: prefix.

test_that("rdLabels errors when Rd entry missing", {
  local_mocked_bindings(
    Rd_db = function(...) list("Other.Rd" = list())
  )
  expect_error(rdLabels("Pizza", "bedrock"), "no Rd entry found")
})

test_that("rdLabels errors when no \\describe section", {
  fake_rd <- list(
    "Pizza.Rd" = structure(list(), class = "Rd")
  )
  local_mocked_bindings(
    Rd_db = function(...) fake_rd
  )
  expect_error(rdLabels("Pizza", "bedrock"), "no \\\\describe section")
})

test_that("rdLabels errors on an empty \\describe section", {
  tf <- tempfile(fileext = ".Rd")
  on.exit(unlink(tf), add = TRUE)
  writeLines(
    c(
      "\\name{EmptyData}",
      "\\alias{EmptyData}",
      "\\title{Empty data}",
      "\\format{A data frame with no documented variables:",
      "\\describe{",
      "}",
      "}"
    ),
    tf
  )

  local_mocked_bindings(Rd_db = function(...)
    list("EmptyData.Rd" = tools::parse_Rd(tf)))

  expect_error(rdLabels("EmptyData", "bedrock"), "no \\\\item entries")
})

test_that("rdLabels errors when an argument is missing", {
  expect_error(rdLabels("Pizza"), "'package' is missing")
  expect_error(rdLabels(), "'dataName' is missing")
})

test_that("rdLabels validates its arguments", {
  # the wording belongs to checkString(), so assert on the argument name
  expect_error(rdLabels(42, "bedrock"), "'dataName'")
  expect_error(rdLabels(c("a", "b"), "bedrock"), "'dataName'")
  expect_error(rdLabels("Pizza", NA_character_), "'package'")
  expect_error(rdLabels("Pizza", c("a", "b")), "'package'")
})


test_that("rdLabels extracts names and descriptions", {
  tf <- tempfile(fileext = ".Rd")
  on.exit(unlink(tf), add = TRUE)
  writeLines(
    c(
      "\\name{ExampleData}",
      "\\alias{ExampleData}",
      "\\title{Example data}",
      "\\format{A data frame with two variables:",
      "\\describe{",
      "  \\item{x}{First variable.}",
      "  \\item{long_name}{Second variable with several words.}",
      "}",
      "}"
    ),
    tf
  )
  
  fake_rd <- list("ExampleData.Rd" = tools::parse_Rd(tf))
  local_mocked_bindings(Rd_db = function(...) fake_rd)
  
  out <- rdLabels("ExampleData", "bedrock")
  
  expect_identical(
    out,
    c(
      x = "First variable.",
      long_name = "Second variable with several words."
    )
  )
})


test_that("rdLabels collapses line breaks in a description", {
  tf <- tempfile(fileext = ".Rd")
  on.exit(unlink(tf), add = TRUE)
  writeLines(
    c(
      "\\name{WrappedData}",
      "\\alias{WrappedData}",
      "\\title{Wrapped data}",
      "\\format{A data frame with one variable:",
      "\\describe{",
      "  \\item{x}{A description that runs",
      "    over two lines.}",
      "}",
      "}"
    ),
    tf
  )

  local_mocked_bindings(Rd_db = function(...)
    list("WrappedData.Rd" = tools::parse_Rd(tf)))

  out <- rdLabels("WrappedData", "bedrock")

  expect_named(out, "x")
  expect_false(grepl("\n", out[["x"]]))
})
