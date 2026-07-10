library(testthat)

# ── unwhich ───────────────────────────────────────────────────────────────────

test_that("unwhich reconstructs TRUE positions", {
  ll <- c(TRUE, FALSE, TRUE, NA, FALSE, FALSE, TRUE)
  expect_equal(unwhich(which(ll), length(ll)),
               c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE))
})

test_that("negative index means TRUE everywhere except", {
  # regression: res[-idx] inverted the logic
  expect_equal(unwhich(-2, 5), c(TRUE, FALSE, TRUE, TRUE, TRUE))
  expect_equal(unwhich(c(-1, -5), 5), c(FALSE, TRUE, TRUE, TRUE, FALSE))
})

test_that("unwhich attaches names on TRUE positions", {
  i <- c(a = 1L, c = 3L)
  res <- unwhich(i, 4)
  expect_equal(names(res), c("a", "", "c", ""))
})

test_that("empty index gives all-FALSE", {
  expect_equal(unwhich(integer(0), 3), rep(FALSE, 3))
})

test_that("mixed signs are rejected", {
  expect_error(unwhich(c(1, -2), 5), "mix")
})

# ── toLong / toWide ───────────────────────────────────────────────────────────

test_that("toLong stacks data frame columns", {
  d <- data.frame(A = 1:2, B = 3:4)
  res <- toLong(d)
  expect_equal(res$groups, c("A", "A", "B", "B"))
  expect_equal(res$x, c(1L, 2L, 3L, 4L))
})

test_that("toLong handles 2d tables as rectangular data", {
  # regression: as.data.frame(table) returned the long freq form
  tab <- as.table(rbind(c(1, 2), c(3, 4)))
  dimnames(tab) <- list(c("r1", "r2"), c("c1", "c2"))
  res <- toLong(tab)
  expect_equal(nrow(res), 4L)
  expect_equal(res$x, c(1, 3, 2, 4))
})

test_that("toLong rejects higher-dimensional tables", {
  expect_error(toLong(HairEyeColor), "2-dimensional")
})

test_that("toWide aligns correctly for >= 10 rows per group", {
  # regression: row.names merge sorted keys as character and
  # misaligned the third group
  set.seed(1)
  n <- 12
  x <- c(1:n, 101:(100 + n), 201:(200 + n))
  g <- rep(c("a", "b", "c"), each = n)
  res <- toWide(x, g)
  expect_equal(res$a, 1:n)
  expect_equal(res$b, 101:(100 + n))
  expect_equal(res$c, 201:(200 + n))
})

test_that("toWide aligns by key", {
  x  <- c(10, 20, 30, 31, 21, 11)
  g  <- c("a", "a", "a", "b", "b", "b")
  by <- c(1, 2, 3, 3, 2, 1)
  res <- toWide(x, g, by = by)
  expect_equal(res$a, c(10, 20, 30))
  expect_equal(res$b, c(11, 21, 31))
})

# ── trim ──────────────────────────────────────────────────────────────────────

test_that("trim attribute is empty when nothing is trimmed", {
  # regression: 1:(lo-1) produced c(1, 0) and a bogus trim index
  x <- 1:10
  res <- trim(x, trim = 0.01)
  expect_equal(as.integer(res), x)
  expect_length(attr(res, "trim"), 0L)
})

test_that("trim removes the k extremes from each end", {
  s <- sample(10:20)
  s.tr <- trim(s, trim = 2)
  expect_length(s.tr, length(s) - 4L)
  expect_setequal(c(s[attr(s.tr, "trim")], s.tr), s)
})

test_that("non-integer count trim is floored", {
  x <- 1:10
  expect_equal(as.integer(trim(x, 2.7)), as.integer(trim(x, 2)))
})

test_that("trim >= n/2 returns NA", {
  expect_true(is.na(trim(1:10, 5)))
  expect_true(is.na(trim(1:10, 0.5)))
})

# ── untable ───────────────────────────────────────────────────────────────────

test_that("untable expands frequencies", {
  res <- untable(c(2, 1), dimnames = list(c("a", "b")))
  expect_equal(as.character(res[, 1]), c("a", "a", "b"))
})

test_that("untable names unnamed dimensions", {
  tab <- as.table(rbind(c(1, 1), c(1, 1)))
  dimnames(tab) <- list(c("x", "y"), c("u", "v"))  # unnamed dims
  res <- untable(tab)
  expect_equal(colnames(res), c("Var1", "Var2"))
  expect_equal(nrow(res), 4L)
})

test_that("untable type accepts function names via match.fun", {
  res <- untable(c(1, 4, 5), dimnames = list(c(5, 10, 15)),
                 type = "as.numeric")[, ]
  expect_equal(res, rep(c(5, 10, 15), times = c(1, 4, 5)))
})

test_that("untable.data.frame expands by frequency column", {
  d.freq <- data.frame(f1 = c("A", "B"), Freq = c(2, 1))
  res <- untable(d.freq)
  expect_equal(as.character(res$f1), c("A", "A", "B"))
})

# ── toBaseR ───────────────────────────────────────────────────────────────────

test_that("toBaseR default warns and returns object unchanged", {
  # regression: returned invisible(NULL), destroying data in pipelines
  d <- data.frame(a = 1:3)
  expect_warning(res <- toBaseR(d), "unchanged")
  expect_identical(res, d)
})
