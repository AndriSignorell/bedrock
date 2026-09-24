
test_that("getConcepts extracts unique sorted concepts", {
  tf <- tempfile(fileext = ".Rd")
  on.exit(unlink(tf), add = TRUE)
  writeLines(
    c(
      "\\name{mock}",
      "\\alias{mock}",
      "\\title{Mock documentation}",
      "\\concept{zeta}",
      "\\concept{number-theory}",
      "\\concept{alpha}",
      "\\concept{zeta}"
    ),
    tf
  )
  
  fakeRd <- list("mock.Rd" = tools::parse_Rd(tf))
  local_mocked_bindings(
    Rd_db = function(...) fakeRd,
    .package = "tools"
  )
  
  concepts <- getConcepts("mockPackage")
  
  expect_identical(concepts, c("alpha", "number-theory", "zeta"))
})

test_that("getConcepts filters and removes a prefix", {
  tf <- tempfile(fileext = ".Rd")
  on.exit(unlink(tf), add = TRUE)
  writeLines(
    c(
      "\\name{mock}",
      "\\alias{mock}",
      "\\title{Mock documentation}",
      "\\concept{number-theory}",
      "\\concept{number-prime}",
      "\\concept{comparison}"
    ),
    tf
  )
  
  fakeRd <- list("mock.Rd" = tools::parse_Rd(tf))
  local_mocked_bindings(
    Rd_db = function(...) fakeRd,
    .package = "tools"
  )
  
  concepts <- getConcepts("mockPackage", prefix = "number-")
  
  expect_identical(concepts, c("prime", "theory"))
})
