
# ── Tests for primes() ───────────────────────────────────────────────────────

# ── return structure ──────────────────────────────────────────────────────────

test_that("single value returns an integer vector", {
  out <- primes(10)
  expect_type(out, "integer")
  expect_null(names(out))
})

test_that("several values return a named list", {
  out <- primes(c(5, 10, 20))
  expect_type(out, "list")
  expect_named(out, c("5", "10", "20"))
})

test_that("list length matches length of input", {
  out <- primes(c(5, 10, 20))
  expect_length(out, 3)
})

# ── known prime sets ──────────────────────────────────────────────────────────

test_that("primes up to 10 are correct", {
  expect_equal(primes(10), c(2L, 3L, 5L, 7L))
})

test_that("primes up to 2 returns only 2", {
  expect_equal(primes(2), 2L)
})

test_that("primes up to 20 are correct", {
  expect_equal(primes(20), c(2L, 3L, 5L, 7L, 11L, 13L, 17L, 19L))
})

# ── edge cases ────────────────────────────────────────────────────────────────

test_that("primes up to 1 returns empty vector", {
  expect_length(primes(1), 0)
})

test_that("primes rejects n < 1", {
  expect_error(primes(0), "positive whole numbers")
})

test_that("n equal to a prime includes that prime", {
  expect_true(7L %in% primes(7))
})

test_that("n equal to a composite excludes it", {
  expect_false(9L %in% primes(9))
})

# ── vectorisation ─────────────────────────────────────────────────────────────

test_that("vectorised call returns independent correct results", {
  out <- primes(c(5, 10))
  expect_equal(out[["5"]],  c(2L, 3L, 5L))
  expect_equal(out[["10"]], c(2L, 3L, 5L, 7L))
})

test_that("single-element input behaves like scalar call", {
  expect_equal(primes(10), primes(c(10)))
})

# ── result elements are integer vectors ───────────────────────────────────────

test_that("each list element is an integer vector", {
  out <- primes(c(20, 30))
  for (el in out)
    expect_type(el, "integer")
})

# ── primality spot checks ─────────────────────────────────────────────────────

test_that("all returned values are actually prime", {
  out <- primes(50)
  expect_true(all(vapply(out, isPrime, logical(1))))
})

test_that("no composite numbers appear in result up to 50", {
  composites <- c(4L, 6L, 8L, 9L, 10L, 12L, 14L, 15L, 16L, 18L,
                  20L, 21L, 22L, 24L, 25L, 26L, 27L, 28L)
  out <- primes(50)
  expect_true(!any(composites %in% out))
})


# -- upper limit --------------------------------------------------------------

test_that("primes enforces its upper limit", {

  # there was no bound at all: the sieve was asked for whatever came in,
  # and `std::vector<bool> is_prime(n + 1, true)` with an int n is signed
  # overflow at .Machine$integer.max
  expect_error(primes(100000001), "must not exceed 100000000")
  expect_error(primes(.Machine$integer.max), "must not exceed")
  expect_error(primes(1e10), "must not exceed")

  # the compiled function is reachable directly and guards itself, which is
  # why it takes a double: converting 1e10 to int would be undefined
  # behaviour BEFORE any check inside could run
  expect_error(bedrock:::primes_upto_cpp(1e10), "between 1 and 100000000")
  expect_error(bedrock:::primes_upto_cpp(0), "between 1 and 100000000")
  expect_error(bedrock:::primes_upto_cpp(2.5), "whole number")
  expect_error(bedrock:::primes_upto_cpp(Inf), "whole number")
})


test_that("primes rejects non-finite values", {

  # Inf %% 1 is NaN, so the old whole-number test evaluated to NA and the
  # surrounding if() failed with a message about the condition
  expect_error(primes(Inf), "positive whole numbers")
  expect_error(primes(-Inf), "positive whole numbers")
  expect_error(primes(NaN), "positive whole numbers")
  expect_error(primes(NA), "positive whole numbers")
  expect_error(primes(2.5), "positive whole numbers")
  expect_error(primes("10"), "positive whole numbers")
})


test_that("primes handles a moderately large limit", {

  out <- primes(1000000)

  expect_length(out, 78498)
  expect_identical(out[[1L]], 2L)
  expect_identical(out[[length(out)]], 999983L)

  # nothing composite slipped through, and nothing prime is missing
  expect_false(is.unsorted(out))
  expect_false(anyDuplicated(out) > 0L)
})


test_that("the sieve agrees with trial division", {

  # the reference is the definition, not the previous implementation
  n <- 500
  expected <- Filter(function(k) all(k %% seq_len(floor(sqrt(k)))[-1] != 0),
                     2:n)

  expect_identical(primes(n), as.integer(expected))
})


test_that("a vector is sieved once, at the largest limit", {

  # lapply() ran a full sieve per element; the results must not change
  out <- primes(c(10, 5, 30))

  expect_named(out, c("10", "5", "30"))
  expect_identical(out[["5"]],  c(2L, 3L, 5L))
  expect_identical(out[["10"]], c(2L, 3L, 5L, 7L))
  expect_identical(out[["30"]], primes(30))

  # order of the input does not matter, and repeats are cheap now
  expect_identical(primes(c(30, 10))[["10"]], primes(10))
  expect_identical(primes(c(100, 100))[[1L]], primes(100))

  # each element is still an independent integer vector
  expect_type(out[["5"]], "integer")
})


test_that("an empty input gives an empty list", {

  out <- primes(integer(0))
  expect_type(out, "list")
  expect_length(out, 0L)
})
