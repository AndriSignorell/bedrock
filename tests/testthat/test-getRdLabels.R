# ------------------------------------------------------------------------------
# rdLabels
# ------------------------------------------------------------------------------

test_that("rdLabels errors when Rd entry missing", {
  mockery::stub(rdLabels, "requireNamespace", function(...) TRUE)
  mockery::stub(rdLabels, "tools::Rd_db",
                function(...) list("Other.Rd" = list()))
  expect_error(rdLabels("Pizza", "bedrock"), "No Rd entry found")
})

test_that("rdLabels errors when no \\describe section", {
  fake_rd <- list(
    "Pizza.Rd" = structure(list(), class = "Rd")
  )
  mockery::stub(rdLabels, "requireNamespace", function(...) TRUE)
  mockery::stub(rdLabels, "tools::Rd_db", function(...) fake_rd)
  expect_error(rdLabels("Pizza", "bedrock"), "No \\\\describe section")
})

test_that("rdLabels errors when bedrock not available", {
  mockery::stub(rdLabels, "requireNamespace", function(...) FALSE)
  expect_error(rdLabels("Pizza", "bedrock"), "bedrock.*required")
})

test_that("rdLabels errors when package argument missing", {
  mockery::stub(rdLabels, "requireNamespace", function(...) TRUE)
  expect_error(rdLabels("Pizza"), "package name")
})