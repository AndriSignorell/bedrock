
test_that("appendEnum prepends roman-lcase column by default", {
  df  <- data.frame(x = 1:3)
  res <- appendEnum(df)
  expect_equal(length(res), 2L)
  expect_equal(res[[1]], c("i. ", "ii. ", "iii. "))
})

test_that("appendEnum roman-ucase", {
  df  <- data.frame(x = 1:3)
  res <- appendEnum(df, type = "roman-ucase")
  expect_equal(res[[1]], c("I. ", "II. ", "III. "))
})

test_that("appendEnum arabic", {
  df  <- data.frame(x = 1:4)
  res <- appendEnum(df, type = "arabic")
  expect_equal(res[[1]], c("1. ", "2. ", "3. ", "4. "))
})

test_that("appendEnum startWith shifts index", {
  df  <- data.frame(x = 1:3)
  res <- appendEnum(df, type = "arabic", startWith = 5L)
  expect_equal(res[[1]], c("5. ", "6. ", "7. "))
})

test_that("appendEnum custom suffix", {
  df  <- data.frame(x = 1:2)
  res <- appendEnum(df, type = "arabic", suffix = ")")
  expect_equal(res[[1]], c("1)", "2)"))
})

test_that("appendEnum colName sets column name", {
  df  <- data.frame(x = 1:2)
  res <- appendEnum(df, colName = "nr")
  expect_equal(names(res)[1], "nr")
})

test_that("appendEnum after inserts at correct position", {
  df  <- data.frame(a = 1:2, b = 3:4)
  res <- appendEnum(df, after = 1L, type = "arabic")
  expect_equal(names(res), c("a", "", "b"))  
})

test_that("appendEnum coerces vector input", {
  res <- appendEnum(1:3, type = "arabic")
  expect_equal(ncol(res), 2L)
})
