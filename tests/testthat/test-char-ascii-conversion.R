

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
