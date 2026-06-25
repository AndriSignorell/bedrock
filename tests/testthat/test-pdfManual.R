
# ------------------------------------------------------------------------------
# pdfManual
# ------------------------------------------------------------------------------

test_that("pdfManual constructs correct CRAN URL", {
  captured <- NULL
  mockery::stub(pdfManual, "browseURL", function(url) { captured <<- url })
  mockery::stub(pdfManual, "requireNamespace", function(...) TRUE)
  pdfManual("stats")
  expect_equal(captured,
               "https://cran.r-project.org/web/packages/stats/stats.pdf")
})

test_that("pdfManual accepts symbol argument", {
  captured <- NULL
  mockery::stub(pdfManual, "browseURL", function(url) { captured <<- url })
  mockery::stub(pdfManual, "requireNamespace", function(...) TRUE)
  pdfManual("stats")   # stats ist immer installiert
  expect_match(captured, "stats/stats\\.pdf$")
})

test_that("pdfManual errors on empty string", {
  expect_error(pdfManual(""), "Invalid package name")
})

test_that("pdfManual warns on uninstalled package", {
  mockery::stub(pdfManual, "browseURL", function(...) invisible(NULL))
  mockery::stub(pdfManual, "requireNamespace", function(...) FALSE)
  expect_warning(pdfManual("nonexistent_pkg_xyz"), "not installed")
})


