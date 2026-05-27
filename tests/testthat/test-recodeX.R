library(testthat)

test_that("recodeX recodes factors", {

  x <- factor(c("a","b","c"))

  res <- recodeX(
    x,
    AB=c("a","b"),
    elseLevel="other"
  )

  expect_true("AB" %in% levels(res))
})

test_that("recodeX keeps specified levels", {

  x <- factor(c("a","b","c"))

  res <- recodeX(
    x,
    keep="a",
    BC=c("b","c")
  )

  expect_true("a" %in% levels(res))
})

test_that("recodeX supports numeric output", {

  x <- factor(c("1","2"))

  res <- recodeX(
    x,
    "10"=1,
    "20"=2,
    num=TRUE
  )

  expect_equal(res, c(10,20))
})

test_that("recodeX errors for duplicated mappings", {

  x <- factor(c("a","b"))

  expect_error(
    recodeX(
      x,
      X=c("a","b"),
      Y=c("b")
    )
  )
})
