# ------------------------------------------------------------------------------
# rdLabels
# ------------------------------------------------------------------------------
# Voraussetzung: requireNamespace() hat ein NULL-Binding im Package
# (R/utils-mockable.R), Rd_db() ist via @importFrom tools importiert
# und wird in rdLabels() OHNE tools::-Prefix aufgerufen.

test_that("rdLabels errors when Rd entry missing", {
  local_mocked_bindings(
    requireNamespace = function(...) TRUE,
    Rd_db            = function(...) list("Other.Rd" = list())
  )
  expect_error(rdLabels("Pizza", "bedrock"), "No Rd entry found")
})

test_that("rdLabels errors when no \\describe section", {
  fake_rd <- list(
    "Pizza.Rd" = structure(list(), class = "Rd")
  )
  local_mocked_bindings(
    requireNamespace = function(...) TRUE,
    Rd_db            = function(...) fake_rd
  )
  expect_error(rdLabels("Pizza", "bedrock"), "No \\\\describe section")
})

test_that("rdLabels errors when bedrock not available", {
  local_mocked_bindings(
    requireNamespace = function(...) FALSE
  )
  expect_error(rdLabels("Pizza", "bedrock"), "bedrock.*required")
})

test_that("rdLabels errors when package argument missing", {
  local_mocked_bindings(
    requireNamespace = function(...) TRUE
  )
  expect_error(rdLabels("Pizza"), "package name")
})
