
# ============================================================
# test-moveAvg.R
# ============================================================

# -----------------------------------------------------------------------
# Basic correctness
# -----------------------------------------------------------------------

test_that("order 1 returns x unchanged", {
  x <- c(1, 3, 5, 7, 9)
  expect_identical(moveAvg(x, 1), x)
})


test_that("right-aligned MA is correct", {
  x   <- c(1, 2, 3, 4, 5)
  res <- moveAvg(x, order = 3, align = "right", endrule = "NA")
  expect_equal(res[3], 2)
  expect_equal(res[4], 3)
  expect_equal(res[5], 4)
  expect_true(all(is.na(res[1:2])))
})


test_that("left-aligned MA is correct", {
  x   <- c(1, 2, 3, 4, 5)
  res <- moveAvg(x, order = 3, align = "left", endrule = "NA")
  expect_equal(res[1], 2)
  expect_equal(res[2], 3)
  expect_equal(res[3], 4)
  expect_true(all(is.na(res[4:5])))
})


test_that("center-aligned odd-order MA is correct", {
  x   <- c(1, 2, 3, 4, 5)
  res <- moveAvg(x, order = 3, align = "center", endrule = "NA")
  expect_equal(res[2], 2)
  expect_equal(res[3], 3)
  expect_equal(res[4], 4)
  expect_true(is.na(res[1]))
  expect_true(is.na(res[5]))
})


test_that("center-aligned even-order MA averages adjacent windows", {
  # order = 4, k2 = 2: z[3..4] filled
  # ma = right-aligned means: (1+2+3+4)/4=2.5, (2+3+4+5)/4=3.5
  # center even: (2.5+3.5)/2 = 3
  x   <- 1:5
  res <- moveAvg(x, order = 4, align = "center", endrule = "NA")
  expect_equal(res[3], 3)
  expect_true(is.na(res[1]))
  expect_true(is.na(res[2]))
  expect_true(is.na(res[5]))
})


test_that("MA of constant series equals that constant", {
  x <- rep(7, 20)
  for (ord in c(1, 3, 5, 7)) {
    res <- moveAvg(x, order = ord, endrule = "NA")
    expect_true(all(res[!is.na(res)] == 7))
  }
})


test_that("MA of linear series: interior values correct", {
  # For a linear series, MA equals the centre value
  x   <- 1:10
  res <- moveAvg(x, order = 3, align = "center", endrule = "NA")
  inner <- res[2:9]
  expect_equal(inner, as.numeric(2:9))
})


# -----------------------------------------------------------------------
# endrule
# -----------------------------------------------------------------------

test_that("endrule = 'NA': boundary positions are NA", {
  x   <- 1:10
  res <- moveAvg(x, order = 3, align = "center", endrule = "NA")
  expect_true(is.na(res[1]))
  expect_true(is.na(res[10]))
})


test_that("endrule = 'keep': boundary positions equal x", {
  x   <- 1:10
  res <- moveAvg(x, order = 3, align = "center", endrule = "keep")
  expect_equal(res[1],  x[1])
  expect_equal(res[10], x[10])
})


test_that("endrule = 'constant': boundary equals nearest MA value", {
  x   <- 1:10
  res_na   <- moveAvg(x, order = 3, align = "center", endrule = "NA")
  res_const <- moveAvg(x, order = 3, align = "center", endrule = "constant")
  
  # first/last non-NA MA value
  first_ma <- res_na[which(!is.na(res_na))[1L]]
  last_ma  <- res_na[tail(which(!is.na(res_na)), 1L)]
  
  expect_equal(res_const[1],  first_ma)
  expect_equal(res_const[10], last_ma)
})


test_that("endrule = 'trim': right-aligned boundary", {
  x   <- c(1, 2, 3, 4, 5)
  res <- moveAvg(x, order = 3, align = "right", endrule = "trim")
  expect_equal(res[1], mean(x[1]))
  expect_equal(res[2], mean(x[1:2]))
  expect_false(anyNA(res))
})


test_that("endrule = 'trim': left-aligned boundary", {
  x   <- c(1, 2, 3, 4, 5)
  res <- moveAvg(x, order = 3, align = "left", endrule = "trim")
  expect_equal(res[5], mean(x[5]))
  expect_equal(res[4], mean(x[4:5]))
  expect_false(anyNA(res))
})


test_that("endrule = 'trim': center-aligned no NAs", {
  x   <- 1:10
  res <- moveAvg(x, order = 5, align = "center", endrule = "trim")
  expect_false(anyNA(res))
  expect_length(res, 10L)
})


# -----------------------------------------------------------------------
# Output length and class
# -----------------------------------------------------------------------

test_that("output length equals input length", {
  x <- rnorm(50)
  for (ord in c(1, 3, 7, 10)) {
    res <- moveAvg(x, order = ord)
    expect_length(res, length(x))
  }
})


test_that("ts input preserves class and tsp", {
  x   <- AirPassengers
  res <- moveAvg(x, order = 5)
  expect_s3_class(res, "ts")
  expect_equal(tsp(res), tsp(x))
})


test_that("named vector preserves names", {
  x       <- c(a = 1, b = 2, c = 3, d = 4, e = 5)
  res     <- moveAvg(x, order = 3, align = "center", endrule = "keep")
  expect_equal(names(res), names(x))
})


# -----------------------------------------------------------------------
# NA propagation
# -----------------------------------------------------------------------

test_that("NA in x propagates to all windows containing it", {
  x      <- c(1, 2, NA, 4, 5)
  res    <- moveAvg(x, order = 3, align = "center", endrule = "NA")
  # positions 2, 3, 4 all have windows containing x[3] = NA
  expect_true(is.na(res[2]))
  expect_true(is.na(res[3]))
  expect_true(is.na(res[4]))
  # positions 1, 5 are already NA from boundary
})


# -----------------------------------------------------------------------
# Input validation
# -----------------------------------------------------------------------

test_that("matrix x raises error", {
  expect_error(moveAvg(matrix(1:9, 3, 3), order = 2), "univariate")
})


test_that("non-numeric x raises error", {
  expect_error(moveAvg(letters[1:5], order = 2), "numeric")
})


test_that("non-integer order raises error", {
  expect_error(moveAvg(1:10, order = 2.5), "positive integer")
})


test_that("order < 1 raises error", {
  expect_error(moveAvg(1:10, order = 0), "positive integer")
})


test_that("order > length(x) raises error", {
  expect_error(moveAvg(1:5, order = 6), "<=")
})


test_that("NA order raises error", {
  expect_error(moveAvg(1:10, order = NA_integer_), "positive integer")
})