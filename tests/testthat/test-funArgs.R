
test_that("funArgs returns argument table", {
  
  res <- funArgs("mean")
  
  expect_true(is.data.frame(res))
  expect_true(all(c("name","value") %in% names(res)))
})

test_that("funArgs works with function objects", {
  
  res <- funArgs(mean)
  
  expect_true(is.data.frame(res))
})

test_that("funArgs sorts arguments", {
  
  res <- funArgs(mean, sorted=TRUE)
  
  expect_true(is.data.frame(res))
})

test_that("funArgs resolves primitives via args() stub", {
  res <- funArgs(sum)
  expect_true(inherits(res, "FunArgs"))
  # sum() has no formals, but args(sum) yields "function (..., na.rm = FALSE)"
  expect_gt(nrow(res), 0L)
  expect_true("na.rm" %in% res$name)
})


test_that("funArgs supports all output formats", {
  f <- function(x, y = 2, ..., z = quote(a + b)) NULL
  
  as_list <- funArgs(f, output = "list")
  as_string <- funArgs(f, output = "string")
  as_data_frame <- funArgs(f)
  
  expect_identical(as_list, formals(f))
  expect_identical(
    as_string,
    "x, y = 2, ..., z = quote(a + b)"
  )
  expect_s3_class(as_data_frame, "FunArgs")
  expect_identical(attr(as_data_frame, "string"), as_string)
})

test_that("funArgs resolves an exported function from a package", {
  out <- funArgs("mean", package = "base", output = "list")
  
  expect_identical(out, formals(base::mean))
})

test_that("funArgs sorts names while keeping dots last", {
  f <- function(z = 1, ..., a, m = 2) NULL
  out <- funArgs(f, sorted = TRUE)
  
  expect_identical(out$name, c("a", "m", "z", "..."))
})

test_that("funArgs handles functions without arguments", {
  out <- funArgs(function() NULL)
  
  expect_s3_class(out, "FunArgs")
  expect_equal(nrow(out), 0L)
  expect_identical(names(out), c("name", "value"))
  expect_identical(funArgs(function() NULL, output = "list"), list())
  expect_identical(funArgs(function() NULL, output = "string"), "")
})

