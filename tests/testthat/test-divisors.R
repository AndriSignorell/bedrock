

test_that("divisors of primes returns only 1", {
  expect_equal(divisors(7),   1L)
  expect_equal(divisors(11),  1L)
  expect_equal(divisors(97),  1L)
})

test_that("divisors of 12", {
  expect_equal(sort(divisors(12)), c(1L, 2L, 3L, 4L, 6L))
  # note: divs() excludes x itself
})

test_that("divisors of known composites", {
  expect_equal(sort(divisors(145)), c(1L, 5L, 29L))
  d786 <- sort(divisors(786))
  expect_true(all(786 %% d786 == 0L))
  expect_true(6L %in% d786)
})

test_that("divisors vectorised over x", {
  res <- divisors(c(12, 7))
  expect_length(res, 2)
  expect_equal(sort(res[[1]]), c(1L, 2L, 3L, 4L, 6L))
  expect_equal(res[[2]], 1L)
})

test_that("divisors of 1 returns integer(0)", {
  expect_equal(divisors(1), integer(0))
})

test_that("divisors errors above integer.max", {
  expect_error(divisors(.Machine$integer.max + 1), "integer.max")
})

