
# ===============================================================
# recycle TESTS
# ===============================================================
# test-recyle.R (typo) was an older copy of this file; the only difference
# was the empty-input test, whose stricter version is kept here

test_that("recycle works with different lengths", {
  res <- recycle(x = 1:5, y = 1, s = letters[1:2])

  expect_equal(lengths(res), c(x = 5L, y = 5L, s = 5L))
  expect_equal(res$y, rep(1, 5))
  expect_equal(res$s, rep(letters[1:2], length.out = 5))
})

test_that("maxdim is inferred from the longest input", {
  expect_equal(attr(recycle(a = 1:3, b = 1:5), "maxdim"), 5)
  expect_equal(attr(recycle(a = 1:4, b = 1), "maxdim"), 4)
})

test_that("explicit maxdim works", {
  res <- recycle(a = 1:2, maxdim = 6)

  expect_equal(length(res$a), 6)
  expect_equal(attr(res, "maxdim"), 6)
})

test_that("recycling repeats correctly", {
  expect_equal(recycle(a = 1:2, maxdim = 5)$a, c(1, 2, 1, 2, 1))
})

test_that("all elements share the same length", {
  res <- recycle(a = 1:3, b = 1, c = letters[1:2])

  expect_true(all(lengths(res) == attr(res, "maxdim")))
})

test_that("strict mode allows length 1 or maxdim only", {
  expect_silent(recycle(a = 1:5, b = 1, strict = TRUE))
  expect_error(recycle(a = 1:5, b = 1:2, strict = TRUE))
})

test_that("invalid maxdim throws error", {
  expect_error(recycle(a = 1:3, maxdim = -1))
  expect_error(recycle(a = 1:3, maxdim = c(1, 2)))
  expect_error(recycle(a = 1:3, maxdim = "a"))
})

test_that("recycle works with Date objects", {
  d <- as.Date("2020-01-01") + 0:2

  res <- recycle(a = d, b = 1)

  expect_s3_class(res$a, "Date")
  expect_equal(length(res$a), 3)
  expect_equal(length(res$b), 3)
})

test_that("names are preserved", {
  expect_true(all(c("x", "y") %in% names(recycle(x = 1:3, y = 1))))
})

test_that("empty input returns an empty list", {
  expect_no_warning(res <- recycle())

  expect_type(res, "list")
  expect_length(res, 0)
  expect_equal(attr(res, "maxdim"), 0L)
})
