

test_that("rdTitle resolves names and aliases", {
  td <- tempfile("rd-title-")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  
  writeLines(
    c(
      "\\name{exampleTopic}",
      "\\alias{exampleTopic}",
      "\\alias{exampleAlias}",
      "\\title{An Example Title}"
    ),
    file.path(td, "exampleTopic.Rd")
  )
  
  expect_identical(rdTitle("exampleTopic", man = td), "An Example Title")
  expect_identical(rdTitle("exampleAlias", man = td), "An Example Title")
})

test_that("rdTitle returns NA for an unknown topic or missing title", {
  td <- tempfile("rd-title-")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  
  writeLines(
    c("\\name{untitled}", "\\alias{untitled}"),
    file.path(td, "untitled.Rd")
  )
  
  expect_identical(rdTitle("untitled", man = td), NA_character_)
  expect_identical(rdTitle("unknown", man = td), NA_character_)
})

test_that("rdTitle validates its inputs", {
  td <- tempfile("rd-title-")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  
  expect_error(rdTitle(c("a", "b"), man = td), "single character string")
  expect_error(rdTitle(1, man = td), "single character string")
  expect_error(rdTitle("a", man = file.path(td, "missing")),
               "Directory not found")
  expect_error(rdTitle("a", man = td), "No \\.Rd files found")
})

