
test_that("revX reverses vectors", {
  expect_equal(revX(1:5), 5:1)
})

test_that("revX reverses matrix rows", {
  m   <- matrix(1:4, nrow = 2)
  res <- revX(m, 1)
  expect_equal(res[1, ], m[2, ])
})

test_that("revX reverses matrix columns", {
  m   <- matrix(1:4, nrow = 2)
  res <- revX(m, 2)
  expect_equal(res[, 1], m[, 2])
})

test_that("revX reverses both margins", {
  m   <- matrix(1:9, nrow = 3)
  res <- revX(m, margin = c(1, 2))
  expect_equal(res[1, 1], m[3, 3])
  expect_equal(res[3, 3], m[1, 1])
})

test_that("revX reverses table", {
  tab <- table(c("a","b","a","b","c"))
  res <- revX(tab, margin = 1)
  expect_equal(names(res)[1], names(tab)[length(tab)])
})

test_that("revX reverses data.frame rows", {
  df  <- data.frame(a = 1:3, b = 4:6)
  res <- revX(df, 1)
  expect_equal(res$a, c(3, 2, 1))
})

test_that("revX reverses data.frame columns", {
  df  <- data.frame(a = 1:3, b = 4:6)
  res <- revX(df, 2)
  expect_equal(names(res)[1], "b")
  expect_equal(names(res)[2], "a")
})

test_that("revX reverses 3d array on dim 3", {
  aa <- array(1:24, dim = c(2, 3, 4))
  res <- revX(aa, 3)
  expect_equal(res[1, 1, 1], aa[1, 1, 4])
})

# ── class and labels ─────────────────────────────────────────────────────────

test_that("the class of the input survives", {
  m <- matrix(1:4, nrow = 2)
  expect_true(is.matrix(revX(m)))

  tab <- table(c("a","b","a"))
  expect_s3_class(revX(tab), "table")

  d <- data.frame(a = 1:3, b = 4:6)
  expect_s3_class(revX(d), "data.frame")
})

test_that("the dimnames travel with the data", {
  tab <- matrix(1:4, nrow = 2,
                dimnames = list(mar1 = c("r1", "r2"), mar2 = c("c1", "c2")))
  res <- revX(tab, margin = c(1, 2))

  expect_equal(rownames(res), c("r2", "r1"))
  expect_equal(colnames(res), c("c2", "c1"))
  # every cell keeps its labels, so this is not a transposition
  expect_equal(res["r1", "c1"], tab["r1", "c1"])
  expect_equal(names(dimnames(res)), c("mar1", "mar2"))
})

test_that("reversing twice gives the original back", {
  aa <- array(1:24, dim = c(2, 3, 4))
  expect_equal(revX(revX(aa)), aa)

  d <- data.frame(a = 1:3, b = 4:6)
  expect_equal(revX(revX(d)), d)
})

# ── degenerate shapes ────────────────────────────────────────────────────────

test_that("a single-column data frame stays a data frame", {
  d <- data.frame(a = 1:3)
  expect_s3_class(revX(d, 1), "data.frame")
  expect_s3_class(revX(d, 2), "data.frame")
  expect_equal(revX(d, 1)$a, c(3L, 2L, 1L))
})

test_that("a zero-row data frame comes back empty", {
  d <- data.frame(a = integer(0), b = character(0))
  res <- revX(d)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 0L)
  expect_equal(names(res), c("b", "a"))
})

test_that("a dimension of extent zero is left empty", {
  m <- matrix(integer(0), nrow = 0, ncol = 2)
  expect_equal(dim(revX(m)), c(0L, 2L))
})

# ── margin validation ────────────────────────────────────────────────────────

test_that("a margin outside the dimensions is an error", {
  m <- matrix(1:4, nrow = 2)
  expect_error(revX(m, 3), "margin")
  expect_error(revX(m, 0), "margin")
  expect_error(revX(m, -1), "margin")
  expect_error(revX(m, 1.5), "margin")
  expect_error(revX(m, NA), "margin")
  expect_error(revX(m, "1"), "margin")
  expect_error(revX(m, integer(0)), "margin")

  d <- data.frame(a = 1:3, b = 4:6)
  expect_error(revX(d, 3), "margin")
  expect_error(revX(d, -1), "margin")
})

test_that("a vector accepts margin = 1 and nothing else", {
  # calling code should not have to know whether its argument has dimensions
  expect_silent(res <- revX(1:5, 1))
  expect_equal(res, 5:1)
  expect_equal(revX(1:5, margin = 1L), 5:1)

  expect_error(revX(1:5, 2), "margin")
  expect_error(revX(1:5, 0), "margin")
})

test_that("the default method warns about arguments it cannot use", {
  expect_warning(revX(1:5, banana = 3), "ignored")
  expect_warning(revX(1:5, 1, 2), "ignored")
})

test_that("a duplicated margin is an error", {
  m <- matrix(1:9, nrow = 3)
  expect_error(revX(m, c(1, 1)), "duplicates")
  expect_error(revX(m, c(1, 2, 1)), "duplicates")

  d <- data.frame(a = 1:3, b = 4:6)
  expect_error(revX(d, c(2, 2)), "duplicates")

  expect_error(revX(1:5, c(1, 1)), "duplicates")
})
