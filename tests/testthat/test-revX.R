library(testthat)

test_that("revX reverses vectors", {

  expect_equal(
    revX(1:5),
    5:1
  )
})

test_that("revX reverses matrix rows", {

  m <- matrix(1:4, nrow=2)

  res <- revX(m, 1)

  expect_equal(res[1,], m[2,])
})

test_that("revX reverses matrix columns", {

  m <- matrix(1:4, nrow=2)

  res <- revX(m, 2)

  expect_equal(res[,1], m[,2])
})

test_that("revX reverses data frame", {

  df <- data.frame(a=1:3, b=4:6)

  res <- revX(df, 1)

  expect_equal(res$a, c(3,2,1))
})
