# ------------------------------------------------------------------------------
# rdLabels
# ------------------------------------------------------------------------------
# Voraussetzung: Rd_db() ist via @importFrom tools importiert und wird in
# rdLabels() OHNE tools::-Prefix aufgerufen.

test_that("rdLabels errors when Rd entry missing", {
  local_mocked_bindings(
    Rd_db = function(...) list("Other.Rd" = list())
  )
  expect_error(rdLabels("Pizza", "bedrock"), "No Rd entry found")
})

test_that("rdLabels errors when no \\describe section", {
  fake_rd <- list(
    "Pizza.Rd" = structure(list(), class = "Rd")
  )
  local_mocked_bindings(
    Rd_db = function(...) fake_rd
  )
  expect_error(rdLabels("Pizza", "bedrock"), "No \\\\describe section")
})

test_that("rdLabels errors when package argument missing", {
  expect_error(rdLabels("Pizza"), "package name")
})
