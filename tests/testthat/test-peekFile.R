

test_that("peekFile reads only the requested number of rows", {
  skip_if_not_installed("readr")
  
  tf <- tempfile(fileext = ".csv")
  on.exit(unlink(tf), add = TRUE)
  writeLines(c("id,value", "1,a", "2,b", "3,c"), tf)
  
  out <- peekFile(tf, n = 2, delim = ",")
  
  expect_s3_class(out, "data.frame")
  expect_false(inherits(out, "tbl_df"))
  expect_equal(nrow(out), 2L)
  expect_identical(out$id, c(1, 2))
})

test_that("peekFile can retain tibble output", {
  skip_if_not_installed("readr")
  
  tf <- tempfile(fileext = ".csv")
  on.exit(unlink(tf), add = TRUE)
  writeLines(c("id,value", "1,a", "2,b"), tf)
  
  out <- peekFile(tf, n = 1, delim = ",", output = "tibble")
  
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 1L)
})

test_that("peekFile ignores internally managed reader arguments", {
  skip_if_not_installed("readr")
  
  tf <- tempfile(fileext = ".csv")
  on.exit(unlink(tf), add = TRUE)
  writeLines(c("id,value", "1,a", "2,b"), tf)
  
  expect_warning(
    out <- peekFile(
      tf,
      n = 1,
      delim = ",",
      n_max = 99,
      show_col_types = TRUE
    ),
    "Ignoring argument"
  )
  expect_equal(nrow(out), 1L)
})

test_that("peekFile validates n and output", {
  skip_if_not_installed("readr")
  
  tf <- tempfile(fileext = ".csv")
  on.exit(unlink(tf), add = TRUE)
  writeLines(c("id", "1"), tf)
  
  for (n in list(0, -1, 1.5, NA_real_, Inf, c(1, 2), "1")) {
    expect_error(
      peekFile(tf, n = n, delim = ","),
      "single positive integer",
      info = paste("n =", paste(n, collapse = ", "))
    )
  }
  
  expect_error(peekFile(tf, output = "matrix"), "arg")
})
