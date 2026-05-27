
# -------------------------------------------------------------------------
# logical
# -------------------------------------------------------------------------
test_that("logical input is converted correctly", {
  expect_equal(asBinary(c(TRUE, FALSE, TRUE)), c(1, 0, 1))
  expect_equal(asBinary(c(FALSE, FALSE)),      c(0, 0))
})

test_that("logical NA is preserved", {
  expect_equal(asBinary(c(TRUE, NA, FALSE)), c(1, NA, 0))
})
# -------------------------------------------------------------------------
# numeric
# -------------------------------------------------------------------------
test_that("valid numeric 0/1 passes through", {
  expect_equal(asBinary(c(0, 1, 0, 1)), c(0, 1, 0, 1))
})

test_that("numeric with NA passes through", {
  expect_equal(asBinary(c(0, NA, 1)), c(0, NA, 1))
})

test_that("numeric with values other than 0/1 throws error", {
  expect_error(asBinary(c(0, 1, 2)), "0, 1, or NA")
})
# -------------------------------------------------------------------------
# factor
# -------------------------------------------------------------------------
test_that("factor default codes second level as 1", {
  x   <- factor(c("A","B","A","B"), levels = c("A","B"))
  res <- suppressWarnings(asBinary(x))
  expect_equal(res, c(0, 1, 0, 1))
})

test_that("factor with explicit ref codes correctly", {
  x <- factor(c("control","treatment","control"), levels = c("control","treatment"))
  expect_equal(asBinary(x, ref = "treatment"), c(0, 1, 0))
  expect_equal(asBinary(x, ref = "control"),   c(1, 0, 1))
})

test_that("factor with wrong ref throws error", {
  x <- factor(c("A","B"))
  expect_error(asBinary(x, ref = "C"), "factor levels")
})

test_that("factor with != 2 levels throws error", {
  x <- factor(c("A","B","C"))
  expect_error(asBinary(x), "2 levels")
})

test_that("factor default produces warning", {
  x <- factor(c("A","B"))
  expect_warning(asBinary(x), "coercing factor")
})

test_that("factor with showWarnings = FALSE produces no warning", {
  x <- factor(c("A","B"))
  expect_no_warning(asBinary(x, showWarnings = FALSE))
})
# -------------------------------------------------------------------------
# character
# -------------------------------------------------------------------------
test_that("character default codes second unique value as 1", {
  x   <- c("no","yes","no","yes")
  res <- suppressWarnings(asBinary(x))
  # second unique value in order of appearance: "yes"
  expect_equal(res, c(0, 1, 0, 1))
})

test_that("character with explicit ref codes correctly", {
  x <- c("F","U","F","U")
  expect_equal(asBinary(x, ref = "F"), c(1, 0, 1, 0))
  expect_equal(asBinary(x, ref = "U"), c(0, 1, 0, 1))
})

test_that("character with wrong ref throws error", {
  expect_error(asBinary(c("A","B"), ref = "C"), "unique values")
})

test_that("character with > 2 unique values throws error", {
  expect_error(asBinary(c("A","B","C")), "2 distinct")
})

test_that("character NA is preserved", {
  x   <- c("A", NA, "B")
  res <- suppressWarnings(asBinary(x))
  expect_true(is.na(res[2]))
})

test_that("character default produces warning", {
  expect_warning(asBinary(c("no","yes")), "coercing character")
})
# -------------------------------------------------------------------------
# unsupported type
# -------------------------------------------------------------------------
test_that("unsupported type throws error", {
  expect_error(asBinary(list(1, 0)), "unsupported type")
})

test_that("complex throws error", {
  expect_error(asBinary(1i), "unsupported type")
})
# -------------------------------------------------------------------------
# names are removed
# -------------------------------------------------------------------------
test_that("names are stripped from output", {
  x <- c(a = TRUE, b = FALSE)
  expect_null(names(asBinary(x)))
})

