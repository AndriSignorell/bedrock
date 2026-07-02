
# ── num ───────────────────────────────────────────────────────────────────────

test_that("num coerces to numeric", {
  expect_equal(num("3.14"), 3.14)
  expect_equal(num(TRUE), 1)
  expect_equal(num(NA), NA_real_)
  expect_true(is.numeric(num(1L)))
})

test_that("num preserves vector length", {
  expect_equal(length(num(1:5)), 5L)
})


# ── int ───────────────────────────────────────────────────────────────────────
test_that("int coerces to integer", {
  expect_equal(int(3.7), 3L)
  expect_equal(int("5"), 5L)
  expect_equal(int(TRUE), 1L)
  expect_equal(int(NA), NA_integer_)
  expect_true(is.integer(int(1.5)))
})


# ── chr ───────────────────────────────────────────────────────────────────────
test_that("chr coerces to character", {
  expect_equal(chr(3.14), "3.14")
  expect_equal(chr(TRUE), "TRUE")
  expect_equal(chr(NA), NA_character_)
  expect_true(is.character(chr(1L)))
})


# ── nchr ──────────────────────────────────────────────────────────────────────
test_that("nchr converts factor to numeric correctly", {
  f <- factor(c("10","20","30"), levels = c("10","20","30"))
  expect_equal(nchr(f), c(10, 20, 30))
  expect_false(identical(as.numeric(f), nchr(f)))  # codes = 1,2,3 vs 10,20,30
})

test_that("nchr handles character input", {
  expect_equal(nchr(c("1.5", "2.5")), c(1.5, 2.5))
})

test_that("nchr returns NA for non-numeric strings", {
  # as.numeric warnt hier ("NAs introduced by coercion") — gewollt,
  # der Wrapper soll die Diagnostik nicht schlucken.
  # Kein Message-Pattern, da die base-Meldung locale-abhaengig ist.
  expect_warning(res <- nchr("abc"))
  expect_true(is.na(res))
})


# ── bin ───────────────────────────────────────────────────────────────────────
test_that("bin converts 0/1 integer to logical", {
  expect_equal(bin(c(0L, 1L, 0L)), c(FALSE, TRUE, FALSE))
})

test_that("bin converts 0/1 numeric to logical", {
  expect_equal(bin(c(0.0, 1.0, 0.0)), c(FALSE, TRUE, FALSE))
})

test_that("bin maps alphabetically first level to FALSE", {
  res <- bin(c("no", "yes", "no"))
  expect_equal(res, c(FALSE, TRUE, FALSE), ignore_attr = TRUE)
})

test_that("bin negation reverses mapping", {
  res <- !bin(c("no", "yes", "no"))
  expect_equal(res, c(TRUE, FALSE, TRUE), ignore_attr = TRUE)
})

test_that("bin works on factor", {
  f <- factor(c("m", "w", "m"))
  expect_equal(bin(f), c(FALSE, TRUE, FALSE), ignore_attr = TRUE)
})

test_that("bin attaches coding attribute", {
  res <- bin(c("no", "yes"))
  expect_false(is.null(attr(res, "coding")))
  expect_equal(names(attr(res, "coding")), c("no", "yes"))
})

test_that("bin returns logical vector", {
  expect_true(is.logical(bin(c(0L, 1L))))
})

test_that("bin handles NA values", {
  res <- bin(c("no", NA, "yes"))
  expect_true(is.na(res[2]))
})


