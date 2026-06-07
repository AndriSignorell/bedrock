
# ── toLong ────────────────────────────────────────────────────────────────────

test_that("toLong reshapes data.frame to long", {
  d <- data.frame(AA = c(1, 2), BB = c(3, 4))
  res <- toLong(d)
  expect_true(is.data.frame(res))
  expect_equal(nrow(res), 4L)
  expect_equal(ncol(res), 2L)
  expect_equal(sort(unique(res[[1]])), c("AA", "BB"))
})

test_that("toLong works with matrix", {
  m <- matrix(1:6, nrow = 2, dimnames = list(NULL, c("X", "Y", "Z")))
  res <- toLong(m)
  expect_equal(nrow(res), 6L)
})

test_that("toLong uses varNames", {
  d <- data.frame(a = 1:3, b = 4:6)
  res <- toLong(d, varNames = c("grp", "val"))
  expect_equal(names(res), c("grp", "val"))
})


# ── toWide ────────────────────────────────────────────────────────────────────

test_that("toWide reshapes to wide format", {
  res <- toWide(PlantGrowth$weight, PlantGrowth$group)
  expect_true(is.data.frame(res))
  expect_equal(ncol(res), 3L)  # ctrl, trt1, trt2
})

test_that("toWide with by argument aligns by key", {
  set.seed(41)
  PlantGrowth$nr <- c(sample(12, 10), sample(12, 10), sample(12, 10))
  res <- toWide(PlantGrowth$weight, PlantGrowth$group, by = PlantGrowth$nr)
  expect_true("by" %in% names(res))
})


