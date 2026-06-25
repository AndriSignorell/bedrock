# ==============================================================================
# Network-dependent functions are tested via mocking (mockery::stub).
# File-dependent functions use tempfile/withr helpers.
# ==============================================================================

# ------------------------------------------------------------------------------
# fileExistURL
# ------------------------------------------------------------------------------

test_that("fileExistURL returns TRUE with status 200", {
  mockery::stub(fileExistURL, "httr::HEAD", list())
  mockery::stub(fileExistURL, "httr::status_code", function(...) 200L)
  res <- fileExistURL("http://example.com/file.csv")
  expect_true(res)
  expect_equal(attr(res, "status"), 200L)
})

test_that("fileExistURL falls back to GET on 405", {
  call_count <- 0L
  mockery::stub(fileExistURL, "httr::HEAD", list())
  mockery::stub(fileExistURL, "httr::GET",  list())
  mockery::stub(fileExistURL, "httr::status_code", function(...) {
    call_count <<- call_count + 1L
    if (call_count == 1L) 405L else 200L
  })
  res <- fileExistURL("http://example.com/file.csv")
  expect_true(res)
})

test_that("fileExistURL returns FALSE with status 404", {
  mockery::stub(fileExistURL, "httr::HEAD", list())
  mockery::stub(fileExistURL, "httr::GET",  list())
  mockery::stub(fileExistURL, "httr::status_code", function(...) 404L)
  res <- fileExistURL("http://example.com/missing.csv")
  expect_false(res)
})

test_that("fileExistURL returns FALSE with error attribute on network failure", {
  mockery::stub(fileExistURL, "httr::HEAD", function(...) stop("network error"))
  res <- fileExistURL("http://unreachable.invalid/file.csv")
  expect_false(res)
  expect_match(attr(res, "error"), "network error")
  expect_true(is.na(attr(res, "status")))
})


