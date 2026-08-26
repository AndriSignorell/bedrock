
# ── isPrime ───────────────────────────────────────────────────────────────────

test_that("isPrime knows the small cases", {
  expect_true(isPrime(2))
  expect_true(isPrime(3))
  expect_false(isPrime(0))
  expect_false(isPrime(1))
  expect_false(isPrime(4))
  expect_equal(isPrime(1:10),
               c(FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE,
                 FALSE, FALSE, FALSE))
})

test_that("isPrime is vectorized and returns logical of the same length", {
  res <- isPrime(c(17, 18, 19))
  expect_type(res, "logical")
  expect_length(res, 3)
  expect_equal(res, c(TRUE, FALSE, TRUE))
  expect_length(isPrime(numeric(0)), 0)
})

test_that("isPrime returns FALSE for irregular input", {
  expect_false(isPrime(2.5))
  expect_false(isPrime(-7))
  expect_false(isPrime(NA_real_))
  expect_false(isPrime(NaN))
  expect_false(isPrime(Inf))
  expect_false(isPrime(-Inf))
  expect_error(isPrime("7"), "numeric")
})

test_that("isPrime agrees with the sieve", {
  p <- primes(1000)
  expect_true(all(isPrime(p)))
  expect_false(any(isPrime(setdiff(2:1000, p))))
})

test_that("isPrime agrees with factorize", {
  # a number is prime iff its factorization is a single factor of
  # multiplicity one
  for (n in 2:500) {
    f <- factorize(n)[[1L]]
    expect_equal(isPrime(n),
                 nrow(f) == 1L && f[1L, "m"] == 1,
                 info = paste("n =", n))
  }
})

test_that("isPrime handles large values below the bound", {
  expect_true(isPrime(982451653))
  expect_true(isPrime(1000000007))
  expect_true(isPrime(999999999989))

  # largest prime not exceeding 2^53
  expect_true(isPrime(9007199254740881))

  # 2^53 itself is admissible - and even
  expect_no_warning(res <- isPrime(2^53))
  expect_false(res)
})


# ── the 2^53 bound ────────────────────────────────────────────────────────────
# Every double above 2^53 is even, so the previous version answered FALSE
# for every prime beyond the bound, without warning.

test_that("isPrime returns NA with a warning above 2^53", {
  expect_warning(res <- isPrime(2^53 + 2), "2\\^53")
  expect_true(is.na(res))
})

test_that("one oversized element does not discard the others", {
  expect_warning(res <- isPrime(c(7, 2^53 + 2, 8)), "2\\^53")
  expect_equal(res, c(TRUE, NA, FALSE))
})

test_that("the warning counts the offending elements once", {
  expect_warning(isPrime(c(2^53 + 2, 2^53 + 4)), "^2 value")
})

test_that("the C++ backend carries the same bound", {
  # reachable by a direct call, so it must not rely on the R wrapper;
  # unqualified, see the note in test-factorize.R
  expect_error(is_prime_cpp(2^53 + 2), "2\\^53")
  expect_true(is_prime_cpp(9007199254740881))
  expect_false(is_prime_cpp(Inf))
  expect_false(is_prime_cpp(2.5))
})
