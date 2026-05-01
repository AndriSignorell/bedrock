
library(testthat)

# ---------------------------
# Helper (reference using filter)
# ---------------------------
ref_ma <- function(x, k, align) {
  if (align == "right") {
    stats::filter(x, rep(1/k, k), sides = 1)
  } else if (align == "left") {
    rev(stats::filter(rev(x), rep(1/k, k), sides = 1))
  } else {
    if (k %% 2 == 1) {
      stats::filter(x, rep(1/k, k), sides = 2)
    } else {
      stats::filter(x,
                    c(1/(2*k), rep(1/k, k-1), 1/(2*k)),
                    sides = 2)
    }
  }
}

# ---------------------------
# Basic correctness
# ---------------------------
test_that("basic moving average matches filter (odd window)", {
  x <- 1:10
  k <- 3
  
  expect_equal(
    as.numeric(moveAvg(x, k, align = "right")),
    as.numeric(ref_ma(x, k, "right"))
  )
})

# ---------------------------
# Align: left / right / center
# ---------------------------
test_that("align = left/right/center works", {
  x <- rnorm(20)
  k <- 5
  
  for (a in c("left", "right", "center")) {
    expect_length(moveAvg(x, k, align = a), length(x))
  }
})

# ---------------------------
# Even window center
# ---------------------------
test_that("even order center behaves correctly", {
  x <- 1:10
  k <- 4
  
  res <- moveAvg(x, k, align = "center")
  
  # center should produce NA at edges
  expect_true(all(is.na(res[1:2])))
  expect_true(all(is.na(res[9:10])))
})

# ---------------------------
# Endrule: keep
# ---------------------------
test_that("endrule = keep preserves original values", {
  x <- rnorm(20)
  k <- 5
  
  res <- moveAvg(x, k, endrule = "keep")
  
  k2 <- k %/% 2
  expect_equal(res[1:k2], x[1:k2])
  expect_equal(res[(length(x)-k2+1):length(x)],
               x[(length(x)-k2+1):length(x)])
})

# ---------------------------
# Endrule: constant
# ---------------------------
test_that("endrule = constant fills edges", {
  x <- rnorm(20)
  k <- 5
  
  res <- moveAvg(x, k, endrule = "constant")
  
  k2 <- k %/% 2
  
  expect_true(all(res[1:k2] == res[k2+1]))
  expect_true(all(res[(length(x)-k2+1):length(x)] ==
                    res[length(x)-k2]))
})

# ---------------------------
# Length and class preserved
# ---------------------------
test_that("length and class preserved", {
  x <- ts(rnorm(30))
  res <- moveAvg(x, 5)
  
  expect_equal(length(res), length(x))
  expect_s3_class(res, "ts")
})

# ---------------------------
# Error handling
# ---------------------------
test_that("invalid order throws error", {
  x <- 1:10
  
  expect_error(moveAvg(x, 0))
  expect_error(moveAvg(x, -1))
  expect_error(moveAvg(x, 2.5))
  expect_error(moveAvg(x, 20))
})

# ---------------------------
# Edge case: order = 1
# ---------------------------
test_that("order = 1 returns original vector", {
  x <- rnorm(10)
  
  expect_equal(moveAvg(x, 1), x)
})

# ---------------------------
# NA propagation (documented behavior)
# ---------------------------
test_that("NA propagates as expected", {
  x <- c(1, 2, NA, 4, 5, 6)
  k <- 3
  
  res <- moveAvg(x, k)
  
  # everything after NA should be NA due to cumsum
  expect_true(any(is.na(res)))
})