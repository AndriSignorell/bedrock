
# ------------------------------------------------------------------------------
# overlap
# ------------------------------------------------------------------------------

test_that("overlap returns the shared length", {
  expect_equal(overlap(c(1, 5), c(3, 7)), 2)
  expect_equal(overlap(c(1, 5), c(1, 5)), 4)   # identical
  expect_equal(overlap(c(1, 2), c(3, 4)), 0)   # disjoint
  expect_equal(overlap(c(1, 3), c(3, 5)), 0)   # touching
})

test_that("overlap sorts reversed bounds silently", {
  expect_equal(overlap(c(5, 1), c(7, 3)), 2)
})

test_that("overlap is vectorized and recycles", {
  x <- matrix(c(1, 5,
                1, 2), ncol = 2, byrow = TRUE)
  y <- matrix(c(3, 7,
                3, 4), ncol = 2, byrow = TRUE)

  expect_equal(overlap(x, y), c(2, 0))
  expect_equal(overlap(x, c(3, 7)), c(2, 0))   # y recycled
  expect_equal(overlap(c(1, 5), y), c(2, 1))   # x recycled
})

# ------------------------------------------------------------------------------
# overlaps (logical)
# ------------------------------------------------------------------------------

test_that("overlaps treats intervals as closed", {
  expect_true(overlaps(c(1, 5), c(3, 7)))
  expect_true(overlaps(c(1, 3), c(3, 5)))      # boundary counts
  expect_false(overlaps(c(1, 2), c(3, 4)))
})

test_that("overlaps is equivalent to a zero distance", {
  # NOT to overlap() > 0: touching intervals have length 0 but do share a
  # point, so overlap() > 0 is strictly stronger (see @details).
  x <- matrix(c(1, 5,
                1, 2,
                1, 3,
                3, 3), ncol = 2, byrow = TRUE)
  y <- matrix(c(3, 7,
                3, 4,
                3, 5,
                3, 3), ncol = 2, byrow = TRUE)

  expect_identical(overlaps(x, y), distance(x, y) == 0)
  expect_identical(overlaps(x, y), c(TRUE, FALSE, TRUE, TRUE))
  expect_true(all(overlaps(x, y) >= (overlap(x, y) > 0)))
})

# ------------------------------------------------------------------------------
# distance
# ------------------------------------------------------------------------------

test_that("distance returns the gap and is symmetric", {
  expect_equal(distance(c(1, 2), c(4, 5)), 2)
  expect_equal(distance(c(4, 5), c(1, 2)), 2)
})

test_that("distance is zero for overlapping and touching intervals", {
  expect_equal(distance(c(1, 5), c(3, 7)), 0)
  expect_equal(distance(c(1, 3), c(3, 5)), 0)
})

test_that("distance is vectorized", {
  x <- matrix(c(1, 2,
                1, 5), ncol = 2, byrow = TRUE)
  y <- matrix(c(4, 5,
                3, 7), ncol = 2, byrow = TRUE)

  expect_equal(distance(x, y), c(2, 0))
})

# ------------------------------------------------------------------------------
# operator
# ------------------------------------------------------------------------------

test_that("%overlaps% wraps overlaps()", {
  expect_true(c(1, 5) %overlaps% c(3, 7))
  expect_false(c(1, 2) %overlaps% c(3, 4))

  m <- matrix(c(1, 5, 2, 6), ncol = 2, byrow = TRUE)
  expect_identical(m %overlaps% c(3, 7), overlaps(m, c(3, 7)))
})

# ------------------------------------------------------------------------------
# naming contract
# ------------------------------------------------------------------------------
# Results are unnamed in every case. Without this, the names would come from
# whichever operand happened to carry dimnames, i.e. they would depend on the
# argument order rather than on the data.

m <- matrix(c(1, 5,
              2, 6), ncol = 2, byrow = TRUE,
            dimnames = list(c("a", "b"), c("lo", "hi")))
v <- c(3, 7)

test_that("results are unnamed when the named matrix comes first", {
  expect_null(names(overlap(m, v)))
  expect_null(names(overlaps(m, v)))
  expect_null(names(distance(m, v)))
  expect_null(names(m %overlaps% v))
})

test_that("results are unnamed when the named matrix comes second", {
  expect_null(names(overlap(v, m)))
  expect_null(names(overlaps(v, m)))
  expect_null(names(distance(v, m)))
  expect_null(names(v %overlaps% m))
})

test_that("results are unnamed for two named matrices and for named vectors", {
  expect_null(names(overlap(m, m)))
  expect_null(names(overlaps(m, m)))
  expect_null(names(distance(m, m)))

  z <- c(lo = 1, hi = 5)                       # vector carrying attributes
  expect_equal(overlap(z, v), 2)
  expect_null(names(overlap(z, v)))
  expect_null(names(overlaps(z, v)))
})

# ------------------------------------------------------------------------------
# edge cases
# ------------------------------------------------------------------------------

test_that("degenerate (zero-length) intervals behave correctly", {
  expect_equal(overlap(c(3, 3), c(3, 3)), 0)
  expect_true(overlaps(c(3, 3), c(3, 3)))
  expect_equal(distance(c(3, 3), c(3, 3)), 0)
  expect_equal(distance(c(3, 3), c(5, 5)), 2)
})

test_that("NA propagates", {
  expect_true(is.na(overlap(c(NA, 5), c(3, 7))))
  expect_true(is.na(overlaps(c(NA, 5), c(3, 7))))
  expect_true(is.na(distance(c(1, 2), c(NA, 5))))
})

test_that("input validation reports the offending argument", {
  expect_error(overlap(c(1, 2, 3), c(1, 2)), "'x'")
  expect_error(overlap(c(1, 2), c(1, 2, 3)), "'y'")
  expect_error(overlap(matrix(1:6, ncol = 3), c(1, 2)), "2 columns")
})
