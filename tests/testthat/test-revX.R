
test_that("revX reverses vectors", {
  expect_equal(revX(1:5), 5:1)
})

test_that("revX reverses matrix rows", {
  m   <- matrix(1:4, nrow = 2)
  res <- revX(m, 1)
  expect_equal(res[1, ], m[2, ])
})

test_that("revX reverses matrix columns", {
  m   <- matrix(1:4, nrow = 2)
  res <- revX(m, 2)
  expect_equal(res[, 1], m[, 2])
})

test_that("revX reverses both margins", {
  m   <- matrix(1:9, nrow = 3)
  res <- revX(m, margin = c(1, 2))
  expect_equal(res[1, 1], m[3, 3])
  expect_equal(res[3, 3], m[1, 1])
})

test_that("revX reverses table", {
  tab <- table(c("a","b","a","b","c"))
  res <- revX(tab, margin = 1)
  expect_equal(names(res)[1], names(tab)[length(tab)])
})

test_that("revX reverses data.frame rows", {
  df  <- data.frame(a = 1:3, b = 4:6)
  res <- revX(df, 1)
  expect_equal(res$a, c(3, 2, 1))
})

test_that("revX reverses data.frame columns", {
  df  <- data.frame(a = 1:3, b = 4:6)
  res <- revX(df, 2)
  expect_equal(names(res)[1], "b")
  expect_equal(names(res)[2], "a")
})

test_that("revX reverses 3d array on dim 3", {
  aa <- array(1:24, dim = c(2, 3, 4))
  res <- revX(aa, 3)
  expect_equal(res[1, 1, 1], aa[1, 1, 4])
})

