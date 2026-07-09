# =========================================================
# test-toBaseR.R
# =========================================================

library(testthat)

test_that("toBaseR.default warns", {

  expect_warning(
    toBaseR(1:5),
    "Not implemented"
  )

})

test_that("toBaseR leaves data.frame unchanged", {

  x <- data.frame(a = 1:3)

  expect_warning(
    res <- toBaseR(x),
    "Not implemented"
  )

  expect_identical(res, x)

})

# ── toBaseR ───────────────────────────────────────────────────────────────────
test_that("toBaseR.default warns and returns object unchanged", {

  expect_warning(
    res <- toBaseR(42),
    "Not implemented"
  )

  expect_identical(res, 42)

})

test_that("tibble is converted to data.frame", {

  skip_if_not_installed("tibble")

  x <- tibble::tibble(
    a = 1:3,
    b = letters[1:3]
  )

  res <- toBaseR(x)

  expect_s3_class(res, "data.frame")
  expect_false(inherits(res, "tbl_df"))
  expect_equal(res$a, 1:3)
  expect_equal(res$b, letters[1:3])

})

test_that("SPSS attributes are removed", {

  skip_if_not_installed("tibble")

  x <- tibble::tibble(
    a = structure(
      1:3,
      format.spss = "F8.2",
      display_width = 10,
      format.stata = "%9.0g"
    )
  )

  res <- toBaseR(x)

  expect_null(attr(res$a, "format.spss"))
  expect_null(attr(res$a, "display_width"))
  expect_null(attr(res$a, "format.stata"))

})

test_that("haven_labelled becomes factor", {

  skip_if_not_installed("haven")

  x <- haven::labelled(
    c(1,2,1),
    labels = c(No = 1, Yes = 2)
  )

  res <- toBaseR(x)

  expect_s3_class(res, "factor")
  expect_equal(levels(res), c("No","Yes"))

})

test_that("all labelled columns are converted", {

  skip_if_not_installed("haven")
  skip_if_not_installed("tibble")

  x <- tibble::tibble(

    a = haven::labelled(
      c(1,2),
      labels = c(No=1, Yes=2)
    ),

    b = haven::labelled(
      c(1,1),
      labels = c(Male=1)
    )

  )

  res <- toBaseR(x)

  expect_true(is.factor(res$a))
  expect_true(is.factor(res$b))

})

test_that("returns plain data.frame", {

  skip_if_not_installed("tibble")

  x <- tibble::tibble(a = 1)

  res <- toBaseR(x)

  expect_true(is.data.frame(res))
  expect_false(inherits(res, "tbl"))

})
