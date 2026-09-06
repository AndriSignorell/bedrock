test_that("locf carries forward the last observed value", {
  x <- c(1, NA, NA, 4)
  expect_equal(locf(x), c(1, 1, 1, 4))
})

test_that("locf preserves leading NAs (nothing to carry forward)", {
  x <- c(NA, 2, NA, 4)
  expect_equal(locf(x), c(NA, 2, 2, 4))
})

test_that("locf handles multiple NA runs correctly", {
  x <- c(1, NA, NA, 4, NA, NA, 7)
  expect_equal(locf(x), c(1, 1, 1, 4, 4, 4, 7))
})

test_that("locf returns input unchanged when no NAs present", {
  x <- c(1, 2, 3, 4)
  expect_equal(locf(x), x)
})

test_that("locf returns all NAs when input is all NAs", {
  x <- rep(NA_real_, 5)
  expect_equal(locf(x), x)
})

test_that("locf preserves the class of the input (numeric)", {
  x <- c(1.5, NA, 3.5)
  expect_type(locf(x), "double")
})

test_that("locf handles NA followed immediately by a value", {
  x <- c(NA, 3, NA)
  expect_equal(locf(x), c(NA, 3, 3))
})

test_that("locf result has same length as input", {
  x <- c(1, NA, 3, NA, NA)
  expect_length(locf(x), length(x))
})


test_that("locf handles data frames column by column", {
  x <- data.frame(
    a = c(NA, 1, NA, 3),
    b = c(10, NA, 20, NA)
  )
  expected <- data.frame(
    a = c(NA, 1, 1, 3),
    b = c(10, 10, 20, 20)
  )
  
  out <- locf(x)
  
  expect_s3_class(out, "data.frame")
  expect_identical(out, expected)
})

test_that("locf handles matrix columns independently", {
  x <- cbind(
    a = c(NA, 1, NA, 3),
    b = c(10, NA, 20, NA)
  )
  expected <- cbind(
    a = c(NA, 1, 1, 3),
    b = c(10, 10, 20, 20)
  )
  
  out <- locf(x)
  
  expect_true(is.matrix(out))
  expect_identical(out, expected)
})

test_that("locf preserves factor levels and ordering", {
  x <- ordered(
    c(NA, "low", NA, "high", NA),
    levels = c("low", "medium", "high")
  )
  
  out <- locf(x)
  
  expect_true(is.ordered(out))
  expect_identical(levels(out), levels(x))
  expect_identical(
    as.character(out),
    c(NA, "low", "low", "high", "high")
  )
})

test_that("locf preserves empty vector types", {
  expect_identical(locf(numeric()), numeric())
  expect_identical(locf(character()), character())
  expect_identical(locf(as.Date(character())), as.Date(character()))
})

