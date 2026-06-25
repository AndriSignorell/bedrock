# ------------------------------------------------------------------------------
# getRdLabels
# ------------------------------------------------------------------------------

test_that("getRdLabels errors when Rd entry missing", {
  mockery::stub(getRdLabels, "requireNamespace", function(...) TRUE)
  mockery::stub(getRdLabels, "tools::Rd_db",
                function(...) list("Other.Rd" = list()))
  expect_error(getRdLabels("Pizza", "bedrock"), "No Rd entry found")
})

test_that("getRdLabels errors when no \\describe section", {
  fake_rd <- list(
    "Pizza.Rd" = structure(list(), class = "Rd")
  )
  mockery::stub(getRdLabels, "requireNamespace", function(...) TRUE)
  mockery::stub(getRdLabels, "tools::Rd_db", function(...) fake_rd)
  expect_error(getRdLabels("Pizza", "bedrock"), "No \\\\describe section")
})

test_that("getRdLabels errors when bedrock not available", {
  mockery::stub(getRdLabels, "requireNamespace", function(...) FALSE)
  expect_error(getRdLabels("Pizza", "bedrock"), "bedrock.*required")
})

test_that("getRdLabels errors when package argument missing", {
  mockery::stub(getRdLabels, "requireNamespace", function(...) TRUE)
  expect_error(getRdLabels("Pizza"), "package name")
})