library(testthat)

# ── sortX.default: vectors ────────────────────────────────────────────────────

test_that("sortX sorts numeric vectors like sort", {
  x <- c(3, 1, 2)
  expect_equal(sortX(x), sort(x))
  expect_equal(sortX(x, decreasing = TRUE), sort(x, decreasing = TRUE))
})

test_that("mixed method sorts naturally", {
  x <- c("A1", "A10", "A2")
  expect_equal(sortX(x, method = "mixed"), c("A1", "A2", "A10"))
})

test_that("mixed method does not drop shorter strings", {
  # regression: padded token slots must not interact with na.last
  x <- c("A10", "A", "A2")
  res <- sortX(x, method = "mixed")
  expect_length(res, 3L)
  expect_equal(res, c("A", "A2", "A10"))
})

test_that("mixed method decreasing reverses natural order", {
  x <- c("A10", "A", "A2")
  expect_equal(sortX(x, method = "mixed", decreasing = TRUE),
               rev(sortX(x, method = "mixed")))
})

test_that("mixed method handles real NAs per na.last", {
  x <- c("A2", NA, "A1")
  # default na.last = NA removes NAs, like sort()
  expect_equal(sortX(x, method = "mixed"), c("A1", "A2"))
  expect_equal(sortX(x, method = "mixed", na.last = TRUE),
               c("A1", "A2", NA))
  expect_equal(sortX(x, method = "mixed", na.last = FALSE),
               c(NA, "A1", "A2"))
})

test_that("factorsAsCharacter applies to method = 'default'", {
  # custom level order differs from alphabetical label order
  f <- factor(c("b", "a", "c"), levels = c("c", "b", "a"))

  # by label (default)
  expect_equal(as.character(sortX(f)), c("a", "b", "c"))

  # by level order
  expect_equal(as.character(sortX(f, factorsAsCharacter = FALSE)),
               c("c", "b", "a"))
})

# ── sortX.data.frame ──────────────────────────────────────────────────────────

test_that("data frame sorts by named column", {
  d <- data.frame(g = c("b", "a", "c"), v = c(2, 3, 1))
  res <- sortX(d, ord = "g")
  expect_equal(res$g, c("a", "b", "c"))
})

test_that("per-column decreasing works", {
  d <- data.frame(g = c("a", "a", "b"), v = c(1, 2, 3))
  res <- sortX(d, ord = c("g", "v"), decreasing = c(FALSE, TRUE))
  expect_equal(res$v, c(2, 1, 3))
})

test_that("unknown column name errors", {
  d <- data.frame(a = 1:3)
  expect_error(sortX(d, ord = "zz"), "Unknown column")
})

test_that("marginal sums are rejected for data frames", {
  d <- data.frame(a = 1:3, b = 4:6)
  expect_error(sortX(d, ord = ncol(d) + 1L), "not supported")
})

# ── sortX.table / matrix ──────────────────────────────────────────────────────

test_that("table sorts by marginal row sums", {
  tab <- as.table(rbind(c(1, 2), c(5, 5), c(0, 1)))
  res <- sortX(tab, ord = ncol(tab) + 1L, decreasing = TRUE)
  expect_equal(unname(rowSums(res)), c(10, 3, 1))
})

test_that("n-dimensional tables are rejected", {
  expect_error(sortX(HairEyeColor), "2-dimensional")
})

test_that("ord = 0L sorts by rownames", {
  m <- matrix(1:4, 2, dimnames = list(c("z", "a"), NULL))
  res <- sortX(m, ord = 0L)
  expect_equal(rownames(res), c("a", "z"))
})
