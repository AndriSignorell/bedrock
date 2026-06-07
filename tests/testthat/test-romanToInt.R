
# ── romanToInt ────────────────────────────────────────────────────────────────

test_that("romanToInt converts basic numerals", {
  expect_equal(romanToInt("I"),    1L,   ignore_attr = TRUE)
  expect_equal(romanToInt("V"),    5L,   ignore_attr = TRUE)
  expect_equal(romanToInt("X"),   10L,   ignore_attr = TRUE)
  expect_equal(romanToInt("L"),   50L,   ignore_attr = TRUE)
  expect_equal(romanToInt("C"),  100L,   ignore_attr = TRUE)
  expect_equal(romanToInt("D"),  500L,   ignore_attr = TRUE)
  expect_equal(romanToInt("M"), 1000L,   ignore_attr = TRUE)
})

test_that("romanToInt handles subtractive notation", {
  expect_equal(romanToInt("IV"),   4L, ignore_attr = TRUE)
  expect_equal(romanToInt("IX"),   9L, ignore_attr = TRUE)
  expect_equal(romanToInt("XL"),  40L, ignore_attr = TRUE)
  expect_equal(romanToInt("XC"),  90L, ignore_attr = TRUE)
  expect_equal(romanToInt("CD"), 400L, ignore_attr = TRUE)
  expect_equal(romanToInt("CM"), 900L, ignore_attr = TRUE)
})

test_that("romanToInt converts compound numerals", {
  expect_equal(romanToInt("XIV"),     14L, ignore_attr = TRUE)
  expect_equal(romanToInt("XLII"),    42L, ignore_attr = TRUE)
  expect_equal(romanToInt("MCMXCIX"), 1999L, ignore_attr = TRUE)
  expect_equal(romanToInt("MMXXIV"),  2024L, ignore_attr = TRUE)
})

test_that("romanToInt is case-insensitive", {
  expect_equal(unname(romanToInt("xiv")),     unname(romanToInt("XIV")))
  expect_equal(unname(romanToInt("mcmxcix")), unname(romanToInt("MCMXCIX")))
})

test_that("romanToInt handles leading/trailing whitespace", {
  expect_equal(unname(romanToInt("  XIV  ")), unname(romanToInt("XIV")))
})

test_that("romanToInt returns NA for invalid input", {
  expect_true(is.na(romanToInt("ABC")))
})

test_that("romanToInt handles NA input", {
  expect_true(is.na(romanToInt(NA)))
})

test_that("romanToInt vectorises correctly", {
  res <- romanToInt(c("I", "II", "III", "IV", "V"))
  expect_equal(unname(res), c(1L, 2L, 3L, 4L, 5L))
})