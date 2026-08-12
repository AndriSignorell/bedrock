

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


test_that("the divisors come back in ascending order", {

  # sqrt-based trial division collects the partners in descending order and
  # reverses them; the old seq_len() walk was ascending by construction, so
  # nothing pinned the order before
  expect_identical(divisors(12), c(1L, 2L, 3L, 4L, 6L))
  expect_identical(divisors(36), c(1L, 2L, 3L, 4L, 6L, 9L, 12L, 18L))
  expect_identical(divisors(100), c(1L, 2L, 4L, 5L, 10L, 20L, 25L, 50L))

  for (x in c(2:60, 145L, 786L))
    expect_false(is.unsorted(divisors(x)), info = paste("x =", x))
})


test_that("divisors agrees with direct enumeration", {

  # the reference is the definition itself, not the previous implementation
  for (x in c(2:200, 145L, 786L, 1024L, 9973L)) {

    expected <- seq_len(x - 1L)
    expected <- expected[x %% expected == 0L]

    expect_identical(divisors(x), expected, info = paste("x =", x))
  }
})


test_that("a perfect square does not list its root twice", {

  # d and x/d coincide at sqrt(x), which the sqrt-based loop has to notice
  for (r in 2:20) {
    d <- divisors(r^2)
    expect_equal(sum(d == r), 1L, info = paste("r =", r))
    expect_false(anyDuplicated(d) > 0L)
  }
})


test_that("the integer limit is reached without allocating a gigabyte", {

  # seq_len(x / 2) meant 1.07e9 integers, about 4 GB, before the first
  # division - and the upper bound permits exactly this input. Trial
  # division to sqrt(x) needs at most 46341 iterations.
  expect_identical(divisors(.Machine$integer.max), 1L)   # 2^31 - 1 is prime

  # the most divisor-rich value below the limit
  d <- divisors(1745944200)
  expect_length(d, 1535L)
  expect_false(is.unsorted(d))
  expect_true(all(1745944200 %% d == 0))
})


test_that("non-finite values are refused rather than turning into NA", {

  # Inf %% 1 is NaN, so the old whole-number test evaluated to NA and the
  # surrounding if() failed with a message about the condition
  expect_error(divisors(Inf), "positive whole numbers")
  expect_error(divisors(-Inf), "positive whole numbers")
  expect_error(divisors(NaN), "positive whole numbers")
  expect_error(divisors(NA), "positive whole numbers")

  expect_error(divisors(0), "positive whole numbers")
  expect_error(divisors(-3), "positive whole numbers")
  expect_error(divisors(2.5), "positive whole numbers")
  expect_error(divisors("12"), "positive whole numbers")
})


test_that("the compiled helper guards itself", {

  # divisors() checks its input, but divs_cpp() is reachable directly
  expect_error(bedrock:::divs_cpp(0L), "positive whole number")
  expect_error(bedrock:::divs_cpp(-5L), "positive whole number")
  expect_identical(bedrock:::divs_cpp(1L), integer(0))
})
