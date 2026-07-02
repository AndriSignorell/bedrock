
# ------------------------------------------------------------------------------
# pdfManual
# ------------------------------------------------------------------------------
# Voraussetzung: requireNamespace() hat ein NULL-Binding im Package
# (R/utils-mockable.R), browseURL() ist via @importFrom utils importiert.

test_that("pdfManual constructs correct CRAN URL", {
  captured <- NULL
  local_mocked_bindings(
    browseURL        = function(url) { captured <<- url },
    requireNamespace = function(...) TRUE
  )
  pdfManual("stats")
  expect_equal(captured,
               "https://cran.r-project.org/web/packages/stats/stats.pdf")
})

test_that("pdfManual accepts symbol argument", {
  captured <- NULL
  local_mocked_bindings(
    browseURL        = function(url) { captured <<- url },
    requireNamespace = function(...) TRUE
  )
  pdfManual("stats")   # stats ist immer installiert
  expect_match(captured, "stats/stats\\.pdf$")
})

test_that("pdfManual errors on empty string", {
  expect_error(pdfManual(""), "Invalid package name")
})

test_that("pdfManual warns on uninstalled package", {
  local_mocked_bindings(
    browseURL        = function(...) invisible(NULL),
    requireNamespace = function(...) FALSE
  )
  expect_warning(pdfManual("nonexistent_pkg_xyz"), "not installed")
})
