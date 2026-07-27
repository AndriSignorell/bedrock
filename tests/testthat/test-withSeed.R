test_that("the same seed gives the same result", {

  expect_identical(withSeed(7, runif(3)), withSeed(7, runif(3)))
  expect_false(identical(withSeed(7, runif(3)), withSeed(8, runif(3))))
})


test_that("the caller's random stream continues undisturbed", {

  # the reference: three draws in a row
  set.seed(1)
  ref <- c(runif(1), runif(1), runif(1))

  # the same three draws, with an unrelated seeded draw in between
  set.seed(1)
  a <- runif(1)
  withSeed(99, runif(1))
  b <- runif(1)
  d <- runif(1)

  expect_identical(c(a, b, d), ref)
})


test_that("the state is restored even when the expression fails", {

  set.seed(1)
  before <- get(".Random.seed", envir = globalenv())

  expect_error(withSeed(99, stop("boom")), "boom")
  expect_identical(get(".Random.seed", envir = globalenv()), before)
})


test_that("a NULL seed leaves the stream alone", {

  set.seed(1)
  ref <- c(runif(1), runif(1))

  set.seed(1)
  expect_identical(c(withSeed(NULL, runif(1)), runif(1)), ref)
})


test_that("an unseeded session stays unseeded", {

  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE))
    rm(".Random.seed", envir = globalenv())

  withSeed(42, runif(1))

  expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
})


test_that("the expression sees the caller's environment", {

  n <- 3
  expect_length(withSeed(1, runif(n)), 3)

  # and it is evaluated once, not twice
  calls <- 0
  count <- function() {
    calls <<- calls + 1
    calls
  }
  withSeed(1, count())
  expect_identical(calls, 1)
})


test_that("an invalid seed is rejected", {

  expect_error(withSeed("a", runif(1)), "single number")
  expect_error(withSeed(c(1, 2), runif(1)), "single number")
  expect_error(withSeed(NA, runif(1)), "single number")
})


test_that("the result is the value of the expression, not of set.seed()", {

  expect_identical(withSeed(1, "hello"), "hello")
  expect_null(withSeed(1, NULL))
})
