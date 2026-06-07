
# ── setNamesX ─────────────────────────────────────────────────────────────────

test_that("setNamesX sets names by default", {
  res <- setNamesX(1:5, letters[1:5])
  expect_equal(names(res), letters[1:5])
})

test_that("setNamesX sets names explicitly", {
  res <- setNamesX(1:5, names = letters[1:5])
  expect_equal(names(res), letters[1:5])
})

test_that("setNamesX sets rownames and colnames simultaneously", {
  m   <- matrix(1:12, nrow = 4)
  res <- setNamesX(m, rownames = LETTERS[1:4], colnames = c("x","y","z"))
  expect_equal(rownames(res), LETTERS[1:4])
  expect_equal(colnames(res), c("x","y","z"))
})

test_that("setNamesX recycles names", {
  res <- setNamesX(1:6, names = c("a","b"))
  expect_equal(names(res), rep(c("a","b"), 3))
})

test_that("setNamesX sets dimnames", {
  tab <- as.table(rbind(c(10, 5), c(3, 8)))
  res <- setNamesX(tab, dimnames = list(r = c("R1","R2"), c = c("C1","C2")))
  expect_equal(dimnames(res)$r, c("R1","R2"))
  expect_equal(dimnames(res)$c, c("C1","C2"))
})

test_that("setNamesX removes names when NULL", {
  x   <- setNamesX(1:3, names = c("a","b","c"))
  res <- setNamesX(x, names = NULL)
  expect_null(names(res))
})
