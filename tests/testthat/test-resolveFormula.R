

# ── Tests for resolveFormula ──────────────────────────────────────────────────

library(testthat)

set.seed(42)
df <- data.frame(
  y   = rnorm(30, mean = 50, sd = 10),
  g2  = rep(c("A", "B"), 15),
  g3  = rep(c("A", "B", "C"), 10),
  trt = rep(c("T1", "T2", "T3"), 10),
  blk = rep(1:10, 3),
  stringsAsFactors = FALSE
)

# ── 1. one.sample ─────────────────────────────────────────────────────────────
test_that("one.sample: y ~ 1", {
  res <- resolveFormula(y ~ 1, data = df)
  expect_equal(res$type, "one.sample")
  expect_true("x" %in% names(res))
  expect_equal(length(res$x), nrow(df))
})

test_that("one.sample: not in allowed raises error", {
  expect_error(
    resolveFormula(y ~ 1, data = df,
                   allowed = "n.sample.independent"),
    "not allowed"
  )
})

# ── 2. two.sample.independent ─────────────────────────────────────────────────
test_that("two.sample.independent: y ~ g (k=2)", {
  res <- resolveFormula(y ~ g2, data = df,
                        allowed = c("two.sample.independent",
                                    "n.sample.independent"))
  expect_equal(res$type, "two.sample.independent")
  expect_true(all(c("x", "y", "group") %in% names(res)))
  expect_equal(length(res$x) + length(res$y), nrow(df))
})

test_that("two.sample.independent: falls back to n.sample if not allowed", {
  res <- resolveFormula(y ~ g2, data = df,
                        allowed = "n.sample.independent")
  expect_equal(res$type, "n.sample.independent")
})

# ── 3. two.sample.dependent ───────────────────────────────────────────────────
test_that("two.sample.dependent: Pair(x, y) ~ 1", {
  df2 <- data.frame(
    pre  = rnorm(15, 50, 10),
    post = rnorm(15, 55, 10)
  )
  res <- resolveFormula(Pair(pre, post) ~ 1, data = df2,
                        allowed = c("one.sample",
                                    "two.sample.dependent"))
  expect_equal(res$type, "two.sample.dependent")
  expect_true(all(c("x", "y") %in% names(res)))
  expect_equal(length(res$x), 15L)
})

test_that("two.sample.dependent: not allowed raises error", {
  df2 <- data.frame(pre = rnorm(15), post = rnorm(15))
  expect_error(
    resolveFormula(Pair(pre, post) ~ 1, data = df2,
                   allowed = "one.sample"),
    "not allowed"
  )
})

# ── 4. n.sample.independent ───────────────────────────────────────────────────
test_that("n.sample.independent: y ~ g (k=3)", {
  res <- resolveFormula(y ~ g3, data = df,
                        allowed = c("two.sample.independent",
                                    "n.sample.independent"))
  expect_equal(res$type, "n.sample.independent")
  expect_true(all(c("x", "group") %in% names(res)))
  expect_equal(nlevels(res$group), 3L)
})

test_that("n.sample.independent: not allowed raises error", {
  expect_error(
    resolveFormula(y ~ g3, data = df,
                   allowed = "two.sample.independent"),
    "not allowed"
  )
})

# ── 5. n.sample.dependent ─────────────────────────────────────────────────────
test_that("n.sample.dependent: y ~ trt | block", {
  res <- resolveFormula(y ~ trt | blk, data = df,
                        allowed = "n.sample.dependent")
  expect_equal(res$type, "n.sample.dependent")
  expect_true(all(c("response", "group", "block") %in% names(res)))
})

test_that("n.sample.dependent: not allowed raises error", {
  expect_error(
    resolveFormula(y ~ trt | blk, data = df,
                   allowed = "n.sample.independent"),
    "not allowed"
  )
})

# ── 6. Edge cases ─────────────────────────────────────────────────────────────
test_that("missing formula raises error", {
  expect_error(resolveFormula(), "missing")
})

test_that("grouping factor with < 2 levels raises error", {
  df$g1 <- "A"
  expect_error(
    resolveFormula(y ~ g1, data = df),
    "at least 2 levels"
  )
})

test_that("formula with > 2 terms raises error", {
  expect_error(
    resolveFormula(y ~ g2 + g3, data = df),
    "response ~ group"
  )
})

test_that("matrix data is coerced to data.frame", {
  m <- as.matrix(df[, c("y", "g2")])
  expect_no_error(
    resolveFormula(y ~ g2, data = m,
                   allowed = c("two.sample.independent",
                               "n.sample.independent"))
  )
})

# ── 7. subset ─────────────────────────────────────────────────────────────────
test_that("subset filters observations correctly", {
  subset_expr <- substitute(g3 != "C")
  res <- resolveFormula(y ~ g2, data = df,
                        subset  = subset_expr,
                        allowed = c("two.sample.independent",
                                    "n.sample.independent"))
  expect_equal(length(res$x) + length(res$y), sum(df$g3 != "C"))
})

# ── 8. na.action ──────────────────────────────────────────────────────────────
test_that("na.action = na.omit removes NAs", {
  df_na      <- df
  df_na$y[c(1, 5, 10)] <- NA
  res <- resolveFormula(y ~ g2, data = df_na,
                        na.action = na.omit,
                        allowed   = c("two.sample.independent",
                                      "n.sample.independent"))
  expect_equal(length(res$x) + length(res$y), nrow(df) - 3L)
})

test_that("na.action = na.pass keeps NAs (default)", {
  df_na      <- df
  df_na$y[1L] <- NA
  res <- resolveFormula(y ~ g2, data = df_na,
                        allowed = c("two.sample.independent",
                                    "n.sample.independent"))
  expect_true(any(is.na(c(res$x, res$y))))
})

# ── 9. data.name ──────────────────────────────────────────────────────────────
test_that("data.name for grouped design", {
  res <- resolveFormula(y ~ g2, data = df,
                        allowed = c("two.sample.independent",
                                    "n.sample.independent"))
  expect_equal(res$data.name, "y ~ g2")
})

test_that("data.name for blocked design contains 'and'", {
  res <- resolveFormula(y ~ trt | blk, data = df,
                        allowed = "n.sample.dependent")
  expect_match(res$data.name, "|", fixed = TRUE)
})

cat("\nAll resolveFormula tests passed.\n")
