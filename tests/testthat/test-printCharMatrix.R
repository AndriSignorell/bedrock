library(testthat)

test_that("printCharMatrix runs invisibly", {

  m <- matrix(c("a","b","c","d"), nrow=2)

  expect_invisible(
    printCharMatrix(m)
  )
})

test_that("printCharMatrix supports left alignment", {

  m <- matrix(c("a","b"), nrow=1)

  expect_invisible(
    printCharMatrix(m, align="left")
  )
})
