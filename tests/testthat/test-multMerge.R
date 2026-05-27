
# ── Tests for multMerge() ────────────────────────────────────────────────────

library(testthat)

# ── helpers ──────────────────────────────────────────────────────────────────

# small helper to avoid dependency on setNamesX
mkdf <- function(data, rnames) {
  df <- as.data.frame(data)
  rownames(df) <- rnames
  df
}

# shared fixtures
a <- mkdf(list(w = 1:3), c("A", "B", "C"))
b <- mkdf(list(x = 4:6), c("B", "C", "D"))
df_c <- mkdf(list(y = 7:9), c("C", "D", "E"))

# ── single data.frame passthrough ────────────────────────────────────────────

test_that("single data.frame is returned unchanged", {
  out <- multMerge(a)
  expect_equal(out, a)
})

# ── basic merge: full outer join (default all.x = all.y = TRUE) ──────────────

test_that("two data.frames are merged with all rows retained", {
  out <- multMerge(a, b)
  expect_s3_class(out, "data.frame")
  expect_setequal(rownames(out), c("A", "B", "C", "D"))
})

test_that("three data.frames are merged with all rows retained", {
  out <- multMerge(a, b, df_c)
  expect_setequal(rownames(out), c("A", "B", "C", "D", "E"))
})


# ── row ordering: left-to-right appearance ───────────────────────────────────

test_that("row order follows left-to-right first appearance", {
  out <- multMerge(a, b, df_c)
  expect_equal(rownames(out), c("A", "B", "C", "D", "E"))
})


# ── inner join (all.x = all.y = FALSE) ───────────────────────────────────────

test_that("inner join retains only matching rows", {
  out <- multMerge(a, b, df_c, all.x = FALSE, all.y = FALSE)
  expect_setequal(rownames(out), "C")
})



# ── NA filling for missing rows ───────────────────────────────────────────────

test_that("non-matching rows are filled with NA", {
  out <- multMerge(a, b)
  # A has no match in b -> x should be NA
  expect_true(is.na(out["A", "x"]))
  # D has no match in a -> w should be NA
  expect_true(is.na(out["D", "w"]))
})


test_that("inner join retains only matching rows", {
  out <- multMerge(a, b, all.x = FALSE, all.y = FALSE)
  expect_setequal(rownames(out), c("B", "C"))
})

# ── column names are unique ───────────────────────────────────────────────────

test_that("duplicate column names are made unique", {
  d1 <- mkdf(list(v = 1:2), c("A", "B"))
  d2 <- mkdf(list(v = 3:4), c("A", "B"))
  out <- multMerge(d1, d2)
  expect_false(anyDuplicated(colnames(out)) > 0)
})

test_that("unique column names are preserved unchanged", {
  out <- multMerge(a, b)
  expect_true("w" %in% colnames(out))
  expect_true("x" %in% colnames(out))
})

# ── merge by column ───────────────────────────────────────────────────────────

test_that("merge by column produces correct result", {
  d1 <- data.frame(key = c("A", "B", "C"), w = 1:3, stringsAsFactors = FALSE)
  d2 <- data.frame(key = c("B", "C", "D"), x = 4:6, stringsAsFactors = FALSE)
  out <- multMerge(d1, d2, by = "key")
  expect_s3_class(out, "data.frame")
  expect_true("key" %in% colnames(out))
  expect_setequal(out$key, c("A", "B", "C", "D"))
})

test_that("merge by column: key column is restored as first column", {
  d1 <- data.frame(key = c("A", "B"), w = 1:2, stringsAsFactors = FALSE)
  d2 <- data.frame(key = c("B", "C"), x = 3:4, stringsAsFactors = FALSE)
  out <- multMerge(d1, d2, by = "key")
  expect_equal(colnames(out)[1], "key")
})

test_that("merge by column: rownames are reset (no residual rownames)", {
  d1 <- data.frame(key = c("A", "B"), w = 1:2, stringsAsFactors = FALSE)
  d2 <- data.frame(key = c("B", "C"), x = 3:4, stringsAsFactors = FALSE)
  out <- multMerge(d1, d2, by = "key")
  expect_equal(rownames(out), as.character(seq_len(nrow(out))))
})

# ── return type ───────────────────────────────────────────────────────────────

test_that("result is always a data.frame", {
  expect_s3_class(multMerge(a, b),     "data.frame")
  expect_s3_class(multMerge(a, b, df_c), "data.frame")
})

test_that("column count equals sum of all unique columns", {
  out <- multMerge(a, b, df_c)
  expect_equal(ncol(out), 3)
})

