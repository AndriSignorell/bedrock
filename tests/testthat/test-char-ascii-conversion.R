

library(testthat)


test_that("charToAscii converts characters to ASCII", {
  expect_equal(
    charToAscii("A"),
    65L
  )
})


test_that("asciiToChar converts ASCII to characters", {
  expect_equal(
    asciiToChar(65),
    "A"
  )
})


test_that("roundtrip conversion works", {
  x <- "Silvia"
  expect_equal(
    paste(asciiToChar(charToAscii(x)), collapse = ""),
    x
  )
})


test_that("charToAscii returns list output", {
  res <- charToAscii(c("A", "BC"), output = "list")
  expect_true(is.list(res))
  expect_length(res, 2)
})


test_that("charToAscii simplifies scalar strings", {
  res <- charToAscii("ABC")
  expect_type(res, "integer")
})


test_that("charToAscii simplifies several one-character strings", {
  expect_identical(
    charToAscii(c("A", "B", "C")),
    c(65L, 66L, 67L)
  )
})

test_that("charToAscii retains mixed-width strings as a list", {
  expect_identical(
    charToAscii(c("A", "BC")),
    list(65L, c(66L, 67L))
  )
  expect_identical(charToAscii(character(), output = "list"), list())
  expect_error(charToAscii("A", output = "matrix"), "arg")
})

test_that("asciiToChar converts vectors including printable boundaries", {
  expect_identical(
    asciiToChar(c(32L, 65L, 126L)),
    c(" ", "A", "~")
  )
  expect_identical(asciiToChar(integer()), character())
})
