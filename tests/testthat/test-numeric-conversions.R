

# ============================================================
# test-numeric-conversions.R
# ============================================================

# -----------------------------------------------------------------------
# hexToDec
# -----------------------------------------------------------------------

test_that("hexToDec: basic values", {
  expect_equal(hexToDec("FF"),  255L)
  expect_equal(hexToDec("0"),   0L)
  expect_equal(hexToDec("1"),   1L)
  expect_equal(hexToDec("10"), 16L)
})

test_that("hexToDec: case-insensitive", {
  expect_equal(hexToDec("ff"), hexToDec("FF"))
  expect_equal(hexToDec("aB"), hexToDec("AB"))
})

test_that("hexToDec: strips leading #", {
  expect_equal(hexToDec("#FF"), hexToDec("FF"))
})

test_that("hexToDec: vectorized", {
  expect_equal(hexToDec(c("FF", "AA", "0")), c(255L, 170L, 0L))
})


# -----------------------------------------------------------------------
# decToHex
# -----------------------------------------------------------------------

test_that("decToHex: returns hexmode", {
  expect_s3_class(decToHex(255), "hexmode")
})

test_that("decToHex: round-trips with hexToDec", {
  x <- c(0, 1, 15, 16, 255, 4096)
  expect_equal(hexToDec(as.character(decToHex(x))), as.integer(x))
})


# -----------------------------------------------------------------------
# octToDec / decToOct
# -----------------------------------------------------------------------

test_that("octToDec: basic values", {
  expect_equal(octToDec("10"),  8L)
  expect_equal(octToDec("77"), 63L)
  expect_equal(octToDec("0"),   0L)
})

test_that("decToOct / octToDec: round-trip", {
  x <- c(0, 1, 7, 8, 63, 64, 255)
  expect_equal(octToDec(as.character(decToOct(x))), as.integer(x))
})


# -----------------------------------------------------------------------
# binToDec / decToBin
# -----------------------------------------------------------------------

test_that("binToDec: basic values", {
  expect_equal(binToDec("0"),        0L)
  expect_equal(binToDec("1"),        1L)
  expect_equal(binToDec("101"),      5L)
  expect_equal(binToDec("11111111"), 255L)
})

test_that("decToBin: basic values", {
  expect_equal(decToBin(0),   "0")
  expect_equal(decToBin(1),   "1")
  expect_equal(decToBin(5),   "101")
  expect_equal(decToBin(255), "11111111")
})

test_that("decToBin / binToDec: round-trip", {
  x <- c(0, 1, 2, 7, 42, 255, 1000)
  expect_equal(binToDec(decToBin(x)), as.integer(x))
})

test_that("decToBin: overflow returns NA", {
  expect_true(is.na(decToBin(536870912)))
})


# -----------------------------------------------------------------------
# baseToBase — input / output format
# -----------------------------------------------------------------------

test_that("baseToBase: returns character vector", {
  res <- baseToBase("FF", from = 16, to = 10)
  expect_type(res, "character")
})

test_that("baseToBase: same length as input", {
  res <- baseToBase(c("A", "B", "C"), from = 16, to = 10)
  expect_length(res, 3L)
})

test_that("baseToBase: NA input -> NA output", {
  res <- baseToBase(c("A", NA, "F"), from = 16, to = 10)
  expect_true(is.na(res[2L]))
  expect_false(is.na(res[1L]))
})

test_that("baseToBase: output is uppercase", {
  res <- baseToBase("255", from = 10, to = 16)
  expect_equal(res, "FF")
})


# -----------------------------------------------------------------------
# baseToBase — correctness
# -----------------------------------------------------------------------

test_that("baseToBase: dec -> hex matches decToHex", {
  x   <- c(0, 1, 15, 255, 4096)
  b2b <- baseToBase(as.character(x), from = 10, to = 16)
  ref <- toupper(as.character(decToHex(x)))
  expect_equal(b2b, ref)
})

test_that("baseToBase: hex -> dec matches hexToDec", {
  x   <- c("0", "1", "F", "FF", "ABC")
  b2b <- as.integer(baseToBase(x, from = 16, to = 10))
  ref <- hexToDec(x)
  expect_equal(b2b, ref)
})

test_that("baseToBase: dec -> bin matches decToBin", {
  x   <- c(0, 1, 5, 42, 255)
  b2b <- baseToBase(as.character(x), from = 10, to = 2)
  ref <- decToBin(x)
  expect_equal(b2b, ref)
})

test_that("baseToBase: bin -> dec matches binToDec", {
  x   <- c("0", "1", "101", "11111111")
  b2b <- as.integer(baseToBase(x, from = 2, to = 10))
  ref <- binToDec(x)
  expect_equal(b2b, ref)
})

test_that("baseToBase: zero in any base is '0'", {
  for (b in c(2, 8, 10, 16, 36))
    expect_equal(baseToBase("0", from = 10, to = b), "0")
})

test_that("baseToBase: base 36 round-trip", {
  x   <- c("0", "9", "A", "Z", "ZZ", "1A3F")
  dec <- baseToBase(x, from = 36, to = 10)
  back <- baseToBase(dec, from = 10, to = 36)
  expect_equal(back, toupper(x))
})

test_that("baseToBase: hex -> binary", {
  expect_equal(baseToBase("F",  from = 16, to = 2), "1111")
  expect_equal(baseToBase("10", from = 16, to = 2), "10000")
})

test_that("baseToBase: case-insensitive input", {
  expect_equal(baseToBase("ff",  from = 16, to = 10),
               baseToBase("FF",  from = 16, to = 10))
  expect_equal(baseToBase("1a3f", from = 16, to = 10),
               baseToBase("1A3F", from = 16, to = 10))
})

test_that("baseToBase: numeric x accepted when from = 10", {
  expect_equal(baseToBase(255L, from = 10, to = 16), "FF")
  expect_equal(baseToBase(c(10, 20), from = 10, to = 2),
               c("1010", "10100"))
})


# -----------------------------------------------------------------------
# baseToBase — width padding
# -----------------------------------------------------------------------

test_that("width pads with leading zeros", {
  expect_equal(baseToBase("5",   from = 10, to = 2, width = 8), "00000101")
  expect_equal(baseToBase("255", from = 10, to = 2, width = 8), "11111111")
})

test_that("width does not truncate values wider than width", {
  # 255 in binary is 8 chars; width = 4 should not truncate
  res <- baseToBase("255", from = 10, to = 2, width = 4)
  expect_equal(res, "11111111")
})

test_that("width = 0 for zero gives '0'", {
  expect_equal(baseToBase("0", from = 10, to = 2, width = 0), "0")
})

test_that("width vectorized over x", {
  res <- baseToBase(c("0", "7", "255"), from = 10, to = 2, width = 8)
  expect_equal(nchar(res), c(8L, 8L, 8L))
})


# -----------------------------------------------------------------------
# baseToBase — input validation
# -----------------------------------------------------------------------

test_that("invalid from raises error", {
  expect_error(baseToBase("10", from = 1,  to = 10), "from")
  expect_error(baseToBase("10", from = 37, to = 10), "from")
  expect_error(baseToBase("10", from = NA, to = 10), "from")
})

test_that("invalid to raises error", {
  expect_error(baseToBase("10", from = 10, to = 1),  "to")
  expect_error(baseToBase("10", from = 10, to = 37), "to")
})

test_that("invalid width raises error", {
  expect_error(baseToBase("10", from = 10, to = 2, width = -1),  "width")
  expect_error(baseToBase("10", from = 10, to = 2, width = 1.5), "width")
})

test_that("numeric x with from != 10 raises error", {
  expect_error(baseToBase(255, from = 16, to = 10), "from = 10")
})

test_that("invalid digit for base raises error", {
  # G (17th letter) is valid in base 17+, but not in base 16
  expect_no_error(baseToBase("G", from = 17, to = 10))
  expect_error(baseToBase("G", from = 16, to = 10))
})

