
test_that("splitX default works", {
  x <- 1:4
  g <- c("a","a","b","b")
  res <- splitX(x, g)
  expect_equal(length(res), 2)
  expect_equal(res$a, c(1L, 2L))
  expect_equal(res$b, c(3L, 4L))
})

test_that("splitX default with drop removes empty levels", {
  x <- 1:4
  g <- factor(c("a","a","b","b"), levels = c("a","b","c"))
  res <- splitX(x, g, drop = TRUE)
  expect_equal(length(res), 2)
})

test_that("splitX formula works", {
  df <- data.frame(y = 1:4, g = c("a","a","b","b"))
  res <- splitX(y ~ g, data = df)
  expect_equal(length(res), 2)
  expect_equal(res$a, c(1L, 2L))
})

test_that("splitX formula with multiple groups", {
  df <- data.frame(
    y  = 1:4,
    g1 = c("a","a","b","b"),
    g2 = c(1, 2, 1, 2)
  )
  res <- splitX(y ~ g1 + g2, data = df)
  expect_equal(length(res), 4)
})

test_that("splitX formula errors for invalid formula", {
  expect_error(splitX(~ g, data = data.frame(g = 1)))
})

test_that("splitX formula attaches data.name attribute", {
  df <- data.frame(y = 1:4, g = c("a","a","b","b"))
  res <- splitX(y ~ g, data = df)
  expect_false(is.null(attr(res, "data.name")))
})

test_that("splitX formula supports subset", {
  df <- data.frame(y = 1:4, g = c("a","a","b","b"))
  res <- splitX(y ~ g, data = df, subset = y > 1)
  expect_equal(res$a, 2L)
  expect_equal(res$b, c(3L, 4L))
})

test_that("splitX formula handles NAs via na.action", {
  df <- data.frame(y = c(1, NA, 3, 4), g = c("a","a","b","b"))
  res <- splitX(y ~ g, data = df, na.action = na.omit)
  expect_equal(res$a, 1)
  expect_equal(res$b, c(3, 4))
})

