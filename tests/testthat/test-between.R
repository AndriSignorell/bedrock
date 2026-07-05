
# ── between ───────────────────────────────────────────────────────────────────
test_that("%[]% includes both endpoints", {
  x <- 1:9
  expect_equal(x %[]% c(3, 5), c(F, F, T, T, T, F, F, F, F))
})

test_that("%[)% excludes right endpoint", {
  x <- 1:9
  expect_equal(x %[)% c(3, 5), c(F, F, T, T, F, F, F, F, F))
})

test_that("%(]% excludes left endpoint", {
  x <- 1:9
  expect_equal(x %(]% c(3, 5), c(F, F, F, T, T, F, F, F, F))
})

test_that("%()% excludes both endpoints", {
  x <- 1:9
  expect_equal(x %()% c(3, 5), c(F, F, F, T, F, F, F, F, F))
})

test_that("%][% is outside operator", {
  x <- 1:9
  expect_equal(x %][% c(3, 5), c(T, T, T, F, T, T, T, T, T))
})

test_that("between handles NAs", {
  x <- c(1, NA, 5)
  res <- x %[]% c(1, 5)
  expect_true(is.na(res[2]))
  expect_true(res[1])
  expect_true(res[3])
})

test_that("between works with characters", {
  x <- letters
  expect_true(all(x[(x %[]% c("d", "h"))] %in% c("d","e","f","g","h")))
})

test_that("between works with ordered factors", {
  x <- ordered(1:9)
  expect_equal(x %[]% c(3, 5), c(F, F, T, T, T, F, F, F, F))
})

test_that("between works with matrix rng", {
  expect_equal(2 %[]% cbind(1:4, 2:5), c(T, T, F, F))
})

test_that("outside operators are exact negations of their between counterparts", {
  x <- 1:9
  rng <- c(3, 5)
  expect_equal(x %][% rng, !(x %()% rng))
  expect_equal(x %](% rng, !(x %(]% rng))
  expect_equal(x %)[% rng, !(x %[)% rng))
  expect_equal(x %)(% rng, !(x %[]% rng))
})

test_that("boundary elements are never outside if between included them", {
  # 3 and 5 are inside for %[]%, hence not outside for %)(%
  expect_false(3 %)(% c(3, 5))
  expect_false(5 %)(% c(3, 5))
  # but outside for %][%, since %()% excluded them
  expect_true(3 %][% c(3, 5))
  expect_true(5 %][% c(3, 5))
})

test_that("from > to yields all FALSE", {
  x <- 1:9
  expect_equal(x %[]% c(5, 3), rep(FALSE, 9))
  expect_equal(x %(]% c(5, 5), rep(FALSE, 9))
})

test_that("unordered factor yields NA", {
  expect_true(all(is.na(factor(1:3) %[]% c(1, 2))))
})

test_that("ordered factor with rng value not in levels throws error", {
  x <- ordered(LETTERS[1:5])
  expect_error(x %[]% c("A", "Z"), "levels of the ordered factor")
})

test_that("ordered factor NA is preserved", {
  x <- ordered(c("A", NA, "C"), levels = LETTERS[1:5])
  res <- x %[]% c("A", "C")
  expect_true(is.na(res[2]))
})

test_that("%:% returns elements between two values", {
  set.seed(4)
  x <- sample(LETTERS, size = 10, replace = TRUE)
  res <- x %:% c("S", "L")
  expect_equal(res[1], "S")
  expect_equal(res[length(res)], "L")
})

test_that("%::% greedy version uses last match", {
  x <- c("B", "A", "X", "K", "S", "K", "G", "L", "K", "V", "K", "Z")
  lazy   <- x %:%  c("A", "K")
  greedy <- x %::% c("A", "K")
  expect_true(length(greedy) >= length(lazy))
})


