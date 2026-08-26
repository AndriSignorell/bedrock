
# ── factorize ─────────────────────────────────────────────────────────────────

test_that("factorize returns list of correct length", {
  res <- factorize(c(12, 47))
  expect_equal(length(res), 2)
})

test_that("factorize of prime returns single factor with exponent 1", {
  res <- factorize(47)[[1]]
  expect_equal(unname(res[, "p"]), 47)
  expect_equal(unname(res[, "m"]), 1)
})

test_that("factorize of 12 returns correct factors", {
  res <- factorize(12)[[1]]
  expect_equal(unname(res[, "p"]), c(2, 3))
  expect_equal(unname(res[, "m"]), c(2, 1))
})

test_that("factorize names result by input", {
  res <- factorize(c(6, 10))
  expect_equal(names(res), c("6", "10"))
})


# ── the defining invariants ───────────────────────────────────────────────────
# These three hold for every admissible n and are what the previous
# pollard_rho() violated: it returned a divisor of n + 1 and so reported
# factors whose product was not n (2582 of the numbers in 2..20000).

test_that("factorize returns canonical prime factorizations", {

  for (n in 2:1000) {

    f <- factorize(n)[[1L]]

    expect_true(
      all(isPrime(f[, "p"])),
      info = paste("n =", n)
    )

    expect_equal(
      prod(f[, "p"]^f[, "m"]),
      n,
      info = paste("n =", n)
    )

    # p strictly increasing, hence each prime in exactly one row. The
    # unsorted output of factor_rec() collapses only after sorting: 63
    # arrives as 3, 7, 3 and would otherwise give two rows for 3.
    #
    # unname() because `[` drops a one-row matrix to a length-one vector
    # and takes its name from the remaining dimension: for a prime n,
    # f[, "p"] is a value named "p". That is R's drop rule, not something
    # factorize() does, and every other assertion here uses unname() for
    # the same reason.
    pf <- unname(f[, "p"])

    expect_identical(
      pf,
      sort(unique(pf)),
      info = paste("n =", n)
    )
  }
})

test_that("factorize is correct for the numbers the old rho got wrong", {
  expect_equal(unname(factorize(21)[[1]][, "p"]), c(3, 7))
  expect_equal(unname(factorize(25)[[1]][, "p"]), 5)
  expect_equal(unname(factorize(25)[[1]][, "m"]), 2)
  expect_equal(unname(factorize(95)[[1]][, "p"]), c(5, 19))
})

test_that("repeated factors from different rho splits are merged", {
  # 63 = 3 * 7 * 3 in the order rho produces it
  f <- factorize(63)[[1]]
  expect_equal(nrow(f), 2)
  expect_equal(unname(f[, "p"]), c(3, 7))
  expect_equal(unname(f[, "m"]), c(2, 1))

  # 35 = 7 * 5 in the order rho produces it
  expect_equal(unname(factorize(35)[[1]][, "p"]), c(5, 7))
})


# ── edge cases ────────────────────────────────────────────────────────────────

test_that("factorize(1) is the empty product", {
  f <- factorize(1)[[1]]
  expect_equal(nrow(f), 0)
  expect_equal(colnames(f), c("p", "m"))
  expect_equal(prod(f[, "p"]^f[, "m"]), 1)
})

test_that("factorize handles the upper end of the documented range", {
  f <- factorize(2^53)[[1]]
  expect_equal(unname(f[, "p"]), 2)
  expect_equal(unname(f[, "m"]), 53)

  # semiprime with both factors near sqrt(2^53), the slowest input class
  f <- factorize(94906249 * 94906249)[[1]]
  expect_equal(unname(f[, "p"]), 94906249)
  expect_equal(unname(f[, "m"]), 2)
})

test_that("factorize returns an empty list for empty input", {
  expect_equal(length(factorize(numeric(0))), 0)
})


# ── argument checks ───────────────────────────────────────────────────────────

test_that("factorize rejects non-finite input by name", {
  # Inf %% 1 is NaN; without the is.finite() check this aborted with
  # "missing value where TRUE/FALSE needed"
  expect_error(factorize(Inf), "finite")
  expect_error(factorize(c(2, NA)), "finite")
  expect_error(factorize(NaN), "finite")
})

test_that("factorize rejects non-integers, zero and negatives", {
  expect_error(factorize(2.5), "whole")
  expect_error(factorize(0), "whole")
  expect_error(factorize(-7), "whole")
  expect_error(factorize("12"), "numeric")
})

test_that("factorize rejects values above 2^53", {
  expect_error(factorize(2^53 + 2), "2\\^53")
})

test_that("the C++ backend carries its own guards", {
  # Called unqualified: testthat evaluates test code with the package
  # namespace in scope, so internal objects resolve without ':::'. The
  # qualified form additionally requires the name to exist in the
  # INSTALLED package, which it need not during load_all().
  expect_error(factor_u64_cpp(0), "whole number")
  expect_error(factor_u64_cpp(2.5), "whole number")
  expect_error(factor_u64_cpp(Inf), "whole number")
  expect_error(factor_u64_cpp(2^53 + 2), "whole number")
  expect_equal(nrow(factor_u64_cpp(1)), 0)
})
