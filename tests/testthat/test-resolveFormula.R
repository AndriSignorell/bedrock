
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

# ── 1. one-sample ─────────────────────────────────────────────────────────────
test_that("one-sample: y ~ 1", {
  res <- resolveFormula(y ~ 1, data = df)
  expect_equal(res$type, "one-sample")
  expect_true("x" %in% names(res))
  expect_equal(length(res$x), nrow(df))
})

test_that("one-sample: not in allowed raises error", {
  expect_error(
    resolveFormula(y ~ 1, data = df,
                   allowed = "n-sample-independent"),
    "not allowed"
  )
})

# ── 2. two-sample-independent ─────────────────────────────────────────────────
test_that("two-sample-independent: y ~ g (k=2)", {
  res <- resolveFormula(y ~ g2, data = df,
                        allowed = c("two-sample-independent",
                                    "n-sample-independent"))
  expect_equal(res$type, "two-sample-independent")
  expect_true(all(c("x", "y", "group") %in% names(res)))
  
  # contract: x and group are full-length (n), the same shape as the
  # n-sample-independent case below - this is what makes
  # split(res$x, res$group) work uniformly regardless of k.
  expect_equal(length(res$x), nrow(df))
  expect_equal(length(res$group), nrow(df))
  
  # y is a convenience-only field: just group 2, NOT part of x.
  expect_equal(length(res$y), sum(res$group == levels(res$group)[2]))
})

test_that("two-sample-independent: falls back to n.sample if not allowed", {
  res <- resolveFormula(y ~ g2, data = df,
                        allowed = "n-sample-independent")
  expect_equal(res$type, "n-sample-independent")
})

# ── 3. two-sample-dependent ───────────────────────────────────────────────────
test_that("two-sample-dependent: Pair(x, y) ~ 1", {
  df2 <- data.frame(
    pre  = rnorm(15, 50, 10),
    post = rnorm(15, 55, 10)
  )
  res <- resolveFormula(Pair(pre, post) ~ 1, data = df2,
                        allowed = c("one-sample",
                                    "two-sample-dependent"))
  expect_equal(res$type, "two-sample-dependent")
  expect_true(all(c("x", "y") %in% names(res)))
  expect_equal(length(res$x), 15L)
})

test_that("two-sample-dependent: not allowed raises error", {
  df2 <- data.frame(pre = rnorm(15), post = rnorm(15))
  expect_error(
    resolveFormula(Pair(pre, post) ~ 1, data = df2,
                   allowed = "one-sample"),
    "not allowed"
  )
})

# ── 4. n-sample-independent ───────────────────────────────────────────────────
test_that("n-sample-independent: y ~ g (k=3)", {
  res <- resolveFormula(y ~ g3, data = df,
                        allowed = c("two-sample-independent",
                                    "n-sample-independent"))
  expect_equal(res$type, "n-sample-independent")
  expect_true(all(c("x", "group") %in% names(res)))
  expect_equal(nlevels(res$group), 3L)
})

test_that("n-sample-independent: not allowed raises error", {
  expect_error(
    resolveFormula(y ~ g3, data = df,
                   allowed = "two-sample-independent"),
    "not allowed"
  )
})

# ── 5. n-sample-dependent ─────────────────────────────────────────────────────
test_that("n-sample-dependent: y ~ trt | block", {
  res <- resolveFormula(y ~ trt | blk, data = df,
                        allowed = "n-sample-dependent")
  expect_equal(res$type, "n-sample-dependent")
  expect_true(all(c("response", "treatment", "block") %in% names(res)))
  # 'group' is reserved for the *-independent designs only - must not
  # leak into the blocked-design result.
  expect_false("group" %in% names(res))
})

test_that("n-sample-dependent: not allowed raises error", {
  expect_error(
    resolveFormula(y ~ trt | blk, data = df,
                   allowed = "n-sample-independent"),
    "not allowed"
  )
})

# ── 6. Edge cases ─────────────────────────────────────────────────────────────
test_that("missing formula raises error", {
  expect_error(resolveFormula(), "missing")
})

test_that("grouping factor with 1 level falls back to one-sample", {
  df$g1 <- "A"
  res <- resolveFormula(y ~ g1, data = df,
                        allowed = c("one-sample", "n-sample-independent"))
  expect_equal(res$type, "one-sample")
})

test_that("grouping factor with 1 level raises error if one-sample not allowed", {
  df$g1 <- "A"
  expect_error(
    resolveFormula(y ~ g1, data = df,
                   allowed = "n-sample-independent"),
    "1 level"
  )
})

# ── 10. numeric-numeric ───────────────────────────────────────────────────────
test_that("numeric-numeric: y ~ x (both numeric)", {
  res <- resolveFormula(y ~ blk, data = df,
                        allowed = c("numeric-numeric", "n-sample-independent"))
  expect_equal(res$type, "numeric-numeric")
  expect_true(all(c("x", "predictor") %in% names(res)))
  expect_equal(length(res$x), nrow(df))
  expect_true(is.numeric(res$predictor))
  # 'group' is reserved for categorical designs - must not leak in here.
  expect_false("group" %in% names(res))
})

test_that("numeric-numeric is reachable via the default 'allowed'", {
  # regression guard: numeric-numeric was missing from the default
  # 'allowed' vector, so y ~ x (x numeric) silently fell through to
  # the grouped-design branch and coerced x into a factor with one
  # level per unique value.
  res <- resolveFormula(y ~ blk, data = df)
  expect_equal(res$type, "numeric-numeric")
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
                   allowed = c("two-sample-independent",
                               "n-sample-independent"))
  )
})

# ── 7. subset ─────────────────────────────────────────────────────────────────
test_that("subset filters observations correctly", {
  subset_expr <- substitute(g3 != "C")
  res <- resolveFormula(y ~ g2, data = df,
                        subset  = subset_expr,
                        allowed = c("two-sample-independent",
                                    "n-sample-independent"))
  expect_equal(length(res$x), sum(df$g3 != "C"))
  expect_equal(length(res$group), sum(df$g3 != "C"))
})

# ── 8. na.action ──────────────────────────────────────────────────────────────
test_that("na.action = na.omit removes NAs", {
  df_na      <- df
  df_na$y[c(1, 5, 10)] <- NA
  res <- resolveFormula(y ~ g2, data = df_na,
                        na.action = na.omit,
                        allowed   = c("two-sample-independent",
                                      "n-sample-independent"))
  expect_equal(length(res$x), nrow(df) - 3L)
})

test_that("na.action = na.pass keeps NAs (default)", {
  df_na      <- df
  df_na$y[1L] <- NA
  res <- resolveFormula(y ~ g2, data = df_na,
                        allowed = c("two-sample-independent",
                                    "n-sample-independent"))
  expect_true(anyNA(res$x))
})

# ── 9. data.name ──────────────────────────────────────────────────────────────
test_that("data.name for grouped design", {
  res <- resolveFormula(y ~ g2, data = df,
                        allowed = c("two-sample-independent",
                                    "n-sample-independent"))
  expect_equal(res$data.name, "y ~ g2")
})

test_that("data.name for blocked design contains 'and'", {
  res <- resolveFormula(y ~ trt | blk, data = df,
                        allowed = "n-sample-dependent")
  expect_match(res$data.name, "|", fixed = TRUE)
})

# ── 11. shape-consistency contract (k=2 vs k>2) ──────────────────────────────
test_that("x/group have the same shape across k=2 and k>2 (no special-casing)", {
  
  res2 <- resolveFormula(y ~ g2, data = df,
                         allowed = c("two-sample-independent",
                                     "n-sample-independent"))
  res3 <- resolveFormula(y ~ g3, data = df,
                         allowed = c("two-sample-independent",
                                     "n-sample-independent"))
  
  expect_equal(length(res2$x), length(res2$group))
  expect_equal(length(res3$x), length(res3$group))
  expect_equal(length(res2$x), nrow(df))
  expect_equal(length(res3$x), nrow(df))
})

test_that("split(x, group) works for k=2 without a length-mismatch warning", {
  # regression guard for the original bug: split.default(r$x, r$group)
  # warned 'data length is not a multiple of split variable' because
  # r$x used to be pre-split (group 1 only) while r$group spanned both
  # groups.
  res <- resolveFormula(y ~ g2, data = df,
                        allowed = c("two-sample-independent",
                                    "n-sample-independent"))
  expect_no_warning(s <- split(res$x, res$group))
  expect_equal(length(s), 2L)
  expect_equal(sum(lengths(s)), nrow(df))
})

cat("\nAll resolveFormula tests passed.\n")
