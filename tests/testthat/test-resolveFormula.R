
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

test_that("a one-sided formula raises error", {
  expect_error(resolveFormula(~ y, data = df), "two-sided")
})

test_that("an unknown design type in 'allowed' raises error", {
  expect_error(
    resolveFormula(y ~ g2, data = df,
                   allowed = c("two-sample-independent", "banane")),
    "banane"
  )
  # no partial matching: an ambiguous prefix is not silently dropped
  expect_error(
    resolveFormula(y ~ g2, data = df, allowed = "two-sample"),
    "invalid design type"
  )
})

test_that("'allowed' must be a non-empty character vector", {
  expect_error(resolveFormula(y ~ g2, data = df, allowed = character(0)),
               "character vector")
  expect_error(resolveFormula(y ~ g2, data = df, allowed = NA_character_),
               "character vector")
  expect_error(resolveFormula(y ~ g2, data = df, allowed = 1:2),
               "character vector")
})

test_that("a grouping factor without any level raises error", {
  # all group labels missing, kept by the default na.pass
  dfNa <- df
  dfNa$gna <- NA_character_
  expect_error(resolveFormula(y ~ gna, data = dfNa), "no non-missing levels")

  # the same when na.omit empties the model frame
  expect_error(resolveFormula(y ~ gna, data = dfNa, na.action = na.omit),
               "no non-missing levels")

  # and when subset filters out every observation
  expect_error(resolveFormula(y ~ g2, data = df, subset = g3 == "Z"),
               "no non-missing levels")
})

test_that("duplicates in 'allowed' are accepted", {
  res <- resolveFormula(y ~ g2, data = df,
                        allowed = c("two-sample-independent",
                                    "two-sample-independent"))
  expect_equal(res$type, "two-sample-independent")
})

test_that("duplicates in 'allowed' do not defeat the regression-only rule", {
  # allowed = "regression" alone forces regression; a duplicate must not
  # change that (identical() compared the vector as a whole)
  expect_equal(resolveFormula(y ~ g2, data = df,
                              allowed = c("regression", "regression"))$type,
               "regression")
  expect_equal(resolveFormula(y ~ 1, data = df,
                              allowed = c("regression", "regression"))$type,
               "regression")
})

test_that("allowed = 'regression' does not cover the blocked syntax", {
  expect_error(resolveFormula(y ~ trt | blk, data = df, allowed = "regression"),
               "not allowed")
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


test_that("formula with multiple predictors resolves as regression", {
  
  r <- resolveFormula(y ~ g2 + g3, data = df)
  
  expect_identical(r$type, "regression")
  expect_identical(r$response, df$y)
  expect_equal(r$mf, model.frame(y ~ g2 + g3, data = df))
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
  res <- resolveFormula(y ~ g2, data = df,
                        subset  = g3 != "C",
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

# ── 9. dataName ───────────────────────────────────────────────────────────────
test_that("dataName for grouped design", {
  res <- resolveFormula(y ~ g2, data = df,
                        allowed = c("two-sample-independent",
                                    "n-sample-independent"))
  expect_equal(res$dataName, "y ~ g2")
})

test_that("dataName for blocked design contains 'and'", {
  res <- resolveFormula(y ~ trt | blk, data = df,
                        allowed = "n-sample-dependent")
  expect_match(res$dataName, "|", fixed = TRUE)
})

test_that("no returned component carries a dot in its name", {
  results <- list(
    resolveFormula(y ~ 1, data = df),
    resolveFormula(y ~ g2, data = df,
                   allowed = c("two-sample-independent",
                               "n-sample-independent")),
    resolveFormula(y ~ g3, data = df, allowed = "n-sample-independent"),
    resolveFormula(y ~ trt | blk, data = df, allowed = "n-sample-dependent"),
    resolveFormula(y ~ blk, data = df, allowed = "numeric-numeric")
  )
  for (res in results)
    expect_false(any(grepl(".", names(res), fixed = TRUE)))
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


# ── 12. subset: base R semantics ─────────────────────────────────────────────
test_that("subset behaves as in model.frame(): expression evaluated in data", {
  res <- resolveFormula(y ~ g2, data = df, subset = y > 50)
  expect_equal(length(res$x), sum(df$y > 50))
  expect_equal(nrow(res$mf),
               nrow(model.frame(y ~ g2, data = df, subset = y > 50)))
})

test_that("subset may refer to a variable outside data", {
  cutoff <- 50
  res <- resolveFormula(y ~ g2, data = df, subset = y > cutoff)
  expect_equal(length(res$x), sum(df$y > cutoff))
})

test_that("a column of data takes precedence over a variable of the same name", {
  y <- rep(0, nrow(df))   # would select nothing if used instead of df$y
  res <- resolveFormula(y ~ g2, data = df, subset = y > 50)
  expect_equal(length(res$x), sum(df$y > 50))
})

test_that("subset accepts a logical or numeric index vector", {
  expect_equal(length(resolveFormula(y ~ g2, data = df, subset = 1:10)$x), 10L)
  sel <- df$g3 == "A"
  expect_equal(length(resolveFormula(y ~ g2, data = df, subset = sel)$x),
               sum(sel))
})

test_that("a quoted subset fails, as in base R", {
  expect_error(resolveFormula(y ~ g2, data = df, subset = quote(g3 != "C")))
  expect_error(model.frame(y ~ g2, data = df, subset = quote(g3 != "C")))
})

test_that("subset works without data", {
  yy <- df$y
  gg <- df$g2
  res <- resolveFormula(yy ~ gg, subset = yy > 50)
  expect_equal(length(res$x), sum(df$y > 50))
})

test_that("subset works for the blocked design", {
  res <- resolveFormula(y ~ trt | blk, data = df, subset = blk <= 5,
                        allowed = "n-sample-dependent")
  expect_equal(length(res$response), sum(df$blk <= 5))
  expect_true(all(res$block <= 5))
})

test_that("subset works with matrix data", {
  m <- cbind(y = df$y, x = df$blk)
  res <- resolveFormula(y ~ x, data = m, subset = x > 5)
  expect_equal(res$type, "numeric-numeric")
  expect_equal(length(res$x), sum(df$blk > 5))
})

test_that("subset is applied before na.action", {
  dfNa <- df
  dfNa$y[1:3] <- NA
  res <- resolveFormula(y ~ g2, data = dfNa, subset = g3 != "C",
                        na.action = na.omit)
  expect_equal(length(res$x), sum(dfNa$g3 != "C" & !is.na(dfNa$y)))
})

test_that("rows point to the retained observations after subset and na.action", {
  dfNa <- df
  dfNa$y[1:3] <- NA
  res <- resolveFormula(y ~ g2, data = dfNa, subset = g3 != "C",
                        na.action = na.omit)
  expect_equal(res$rows, which(dfNa$g3 != "C" & !is.na(dfNa$y)))
  expect_equal(dfNa$y[res$rows], res$x, ignore_attr = TRUE)
})

test_that("na.pass is the default, na.omit must be requested", {
  dfNa <- df
  dfNa$y[1:3] <- NA
  expect_equal(length(resolveFormula(y ~ g2, data = dfNa)$x), nrow(df))
})


# ── 13. cells: y ~ a:b ───────────────────────────────────────────────────────
test_that("y ~ a:b combines the cells into one grouping factor", {
  res <- resolveFormula(y ~ g2:g3, data = df,
                        allowed = c("two-sample-independent",
                                    "n-sample-independent"))
  expect_equal(res$type, "n-sample-independent")
  expect_equal(nlevels(res$group), 6L)
  expect_equal(length(res$group), nrow(df))
  expect_setequal(levels(res$group),
                  c("A:A", "A:B", "A:C", "B:A", "B:B", "B:C"))
  expect_equal(as.character(res$group), paste(df$g2, df$g3, sep = ":"))
})

test_that("y ~ a:b keeps only non-empty cells", {
  # only three of the four combinations occur
  dfc <- data.frame(y = 1:6, a = rep(c("A", "B"), each = 3),
                    b = c("x", "x", "y", "y", "y", "y"))
  res <- resolveFormula(y ~ a:b, data = dfc, allowed = "n-sample-independent")
  expect_setequal(levels(res$group), c("A:x", "A:y", "B:y"))
})

test_that("y ~ a:b with two cells is two-sample-independent", {
  dfc <- data.frame(y = 1:4, a = c("A", "A", "B", "B"), b = "x")
  res <- resolveFormula(y ~ a:b, data = dfc,
                        allowed = c("two-sample-independent",
                                    "n-sample-independent"))
  expect_equal(res$type, "two-sample-independent")
  expect_equal(nlevels(res$group), 2L)
})

test_that("y ~ a:b treats a numeric component as categorical", {
  res <- resolveFormula(len ~ supp:dose, data = ToothGrowth,
                        allowed = "n-sample-independent")
  expect_equal(res$type, "n-sample-independent")
  expect_equal(nlevels(res$group), 6L)
  expect_true("OJ:0.5" %in% levels(res$group))
})

test_that("y ~ a:b:c is a single term and gives the cells of three variables", {
  res <- resolveFormula(y ~ g2:g3:trt, data = df,
                        allowed = "n-sample-independent")
  expect_equal(res$type, "n-sample-independent")
  expect_equal(length(res$group), nrow(df))
})

test_that("y ~ a:b combined with subset", {
  res <- resolveFormula(len ~ supp:dose, data = ToothGrowth, subset = len > 10,
                        allowed = "n-sample-independent")
  expect_equal(length(res$x), sum(ToothGrowth$len > 10))
  expect_equal(sum(table(res$group)), sum(ToothGrowth$len > 10))
})

test_that("a cell is missing as soon as one component is missing", {
  dfNa <- df
  dfNa$g2[1:3] <- NA
  res <- resolveFormula(y ~ g2:g3, data = dfNa,
                        allowed = "n-sample-independent")
  expect_equal(sum(is.na(res$group)), 3L)
  expect_false(any(grepl("NA", levels(res$group))))
})

test_that("y ~ a + b is not read as cells", {
  expect_error(resolveFormula(y ~ g2 + g3, data = df,
                              allowed = "n-sample-independent"),
               "a:b")
})

test_that("y ~ a * b is not read as cells", {
  expect_error(resolveFormula(y ~ g2 * g3, data = df,
                              allowed = "n-sample-independent"),
               "a:b")
})

test_that("with regression allowed, y ~ a:b is regression", {
  res <- resolveFormula(y ~ g2:g3, data = df)
  expect_equal(res$type, "regression")
  expect_true("terms" %in% names(res))
  res <- resolveFormula(y ~ g2:g3, data = df,
                        allowed = c("regression", "n-sample-independent"))
  expect_equal(res$type, "regression")
})

test_that("cells are not taken for numeric-numeric", {
  dfn <- data.frame(y = rnorm(20), a = rep(1:2, 10), b = rep(1:4, 5))
  res <- resolveFormula(y ~ a:b, data = dfn,
                        allowed = c("numeric-numeric", "n-sample-independent"))
  expect_equal(res$type, "n-sample-independent")
})

test_that("a single numeric predictor remains numeric-numeric", {
  res <- resolveFormula(len ~ dose, data = ToothGrowth,
                        allowed = c("numeric-numeric", "n-sample-independent"))
  expect_equal(res$type, "numeric-numeric")
})


# ── 14. resolveFormulaFromCall ───────────────────────────────────────────────

# wrappers as a formula method would look, S3 dispatch included
.rfWrap <- function(x, ...) UseMethod(".rfWrap")
.rfWrap.formula <- function(formula, data, subset, na.action = na.omit, ...)
  resolveFormulaFromCall(
    allowed   = c("two-sample-independent", "n-sample-independent"),
    na.action = na.action)

.rfPlain <- function(formula, data, subset, na.action = na.pass)
  resolveFormulaFromCall(na.action = na.action)

test_that("resolveFormulaFromCall() gives the same result as resolveFormula()", {
  expect_equal(.rfWrap(y ~ g2, data = df),
               resolveFormula(y ~ g2, data = df, na.action = na.omit,
                              allowed = c("two-sample-independent",
                                          "n-sample-independent")))
  expect_equal(.rfPlain(y ~ trt | blk, data = df)$type, "n-sample-dependent")
})

test_that("resolveFormulaFromCall() works through S3 dispatch", {
  res <- .rfWrap(y ~ g3, df)
  expect_equal(res$type, "n-sample-independent")
})

test_that("resolveFormulaFromCall() passes subset on unevaluated", {
  res <- .rfWrap(y ~ g2, data = df, subset = g3 != "C")
  expect_equal(length(res$x), sum(df$g3 != "C"))
})

test_that("resolveFormulaFromCall() matches arguments in any order", {
  res <- .rfWrap(subset = g3 != "C", data = df, y ~ g2)
  expect_equal(length(res$x), sum(df$g3 != "C"))
})

test_that("resolveFormulaFromCall(): subset sees the variables of the wrapper's caller", {
  f <- function(k) .rfWrap(y ~ g2, data = df, subset = y > k)
  expect_equal(length(f(50)$x), sum(df$y > 50))
})

test_that("resolveFormulaFromCall(): data may be local to the wrapper's caller", {
  f <- function() {
    d <- df[df$g3 == "A", ]
    .rfWrap(y ~ g2, d, subset = y > 50)
  }
  expect_equal(length(f()$x), sum(df$g3 == "A" & df$y > 50))
})

test_that("resolveFormulaFromCall() works without data", {
  f <- function() {
    yy <- df$y
    gg <- df$g2
    .rfWrap(yy ~ gg, subset = yy > 50)
  }
  expect_equal(length(f()$x), sum(df$y > 50))
})

test_that("resolveFormulaFromCall(): a formula passed on as a variable works", {
  fo  <- y ~ g2
  res <- .rfWrap(fo, df, subset = g3 != "C")
  expect_equal(length(res$x), sum(df$g3 != "C"))
})

test_that("resolveFormulaFromCall() forwards the wrapper's na.action", {
  dfNa <- df
  dfNa$y[1:3] <- NA
  # wrapper default na.omit, not the na.pass default of resolveFormula()
  expect_equal(length(.rfWrap(y ~ g2, dfNa)$x), nrow(df) - 3L)
  # an explicit na.action of the user
  expect_equal(length(.rfWrap(y ~ g2, dfNa, na.action = na.pass)$x), nrow(df))
})

test_that("resolveFormulaFromCall() forwards 'allowed', all types if omitted", {
  expect_error(.rfWrap(y ~ blk, df), "numeric")
  expect_equal(.rfPlain(y ~ blk, df)$type, "numeric-numeric")
})

test_that("resolveFormulaFromCall() handles a:b and its combination with subset", {
  res <- .rfWrap(len ~ supp:dose, ToothGrowth, subset = len > 10)
  expect_equal(nlevels(res$group), 6L)
  expect_equal(length(res$x), sum(ToothGrowth$len > 10))
})

test_that("resolveFormulaFromCall() works when the wrapper is called from another wrapper", {
  # as plot.Desc.nq() calls plotBox(response ~ group): the variables live in
  # the frame of the outer function
  outer <- function(obj) {
    response <- obj$y
    group    <- obj$g3
    .rfWrap(response ~ group)
  }
  res <- outer(df)
  expect_equal(nlevels(res$group), 3L)
  expect_equal(length(res$x), nrow(df))
})

test_that("resolveFormulaFromCall() at top level raises error", {
  # the function object itself is inlined, so the test does not depend on
  # the package being attached to the search path
  expect_error(eval(as.call(list(resolveFormulaFromCall)), globalenv()),
               "within a function")
})

test_that("resolveFormulaFromCall() requires a formula argument in the caller", {
  f <- function(x, data) resolveFormulaFromCall()
  expect_error(f(df$y, df), "no 'formula' argument")
})


# ── 15. offsets ───────────────────────────────────────────────────────────────
test_that("an offset is not taken for a cell component", {
  # offset(dose) is in the model frame but not a term: y ~ supp + offset(dose)
  # was read as the cells of supp and dose
  expect_error(resolveFormula(len ~ supp + offset(dose), data = ToothGrowth,
                              allowed = "n-sample-independent"),
               "offset")
})

test_that("an offset is not taken for a numeric predictor", {
  expect_error(resolveFormula(len ~ offset(dose), data = ToothGrowth,
                              allowed = c("numeric-numeric", "one-sample")),
               "offset")
})

test_that("an offset next to an interaction is rejected", {
  expect_error(resolveFormula(len ~ supp:dose + offset(dose), data = ToothGrowth,
                              allowed = "n-sample-independent"),
               "offset")
})

test_that("an offset is kept for regression", {
  res <- resolveFormula(len ~ supp + offset(dose), data = ToothGrowth,
                        allowed = "regression")
  expect_equal(res$type, "regression")
  expect_false(is.null(attr(res$terms, "offset")))
})

test_that("offsets are rejected in blocked designs", {
  # the blocked branch returns before the general offset check
  expect_error(
    resolveFormula(y ~ trt | offset(blk), data = df,
                   allowed = "n-sample-dependent"),
    "offset"
  )
  expect_error(
    resolveFormula(y ~ offset(trt) | blk, data = df,
                   allowed = "n-sample-dependent"),
    "offset"
  )
})
