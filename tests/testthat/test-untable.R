library(testthat)

test_that("untable expands vectors", {

  x <- c(A=2, B=3)

  res <- untable(x)

  expect_equal(nrow(res), 5)
})

test_that("untable handles numeric conversion", {

  x <- c("5"=1, "10"=2)

  res <- untable(x, type="as.numeric")

  expect_true(is.numeric(res[,1]))
})

test_that("untable expands tables", {

  tab <- table(
    a=c("x","x","y"),
    b=c("u","v","u")
  )

  res <- untable(tab)

  expect_true(is.data.frame(res))
})

test_that("untable.data.frame works", {

  df <- data.frame(
    g=c("A","B"),
    Freq=c(2,3)
  )

  res <- untable(df)

  expect_equal(nrow(res), 5)
})

test_that("untable.data.frame errors for missing freq", {

  df <- data.frame(a=1:3)

  expect_error(
    untable(df)
  )
})

test_that("untable warns for NA", {

  expect_warning(
    untable(c(1, NA))
  )
})
