
# ── resolveContingency ────────────────────────────────────────────────────────

test_that("resolveContingency works with matrix input", {
  m <- matrix(c(10, 5, 3, 8), nrow = 2)
  res <- resolveContingency(m)
  expect_equal(res$n, 26)
  expect_equal(res$r, 2L)
  expect_equal(res$c, 2L)
})

test_that("resolveContingency works with two vectors", {
  x <- c("A","A","B","B")
  y <- c("X","Y","X","Y")
  res <- resolveContingency(x, y)
  expect_equal(res$r, 2L)
  expect_equal(res$c, 2L)
})

test_that("resolveContingency stops on non-numeric matrix", {
  m <- matrix(c("a","b","c","d"), nrow = 2)
  expect_error(resolveContingency(m), "numeric")
})

test_that("resolveContingency warns on non-integer counts", {
  m <- matrix(c(1.5, 2.5, 3.5, 4.5), nrow = 2)
  expect_warning(resolveContingency(m), "non-integer")
})

test_that("integerCounts = FALSE accepts fractional counts silently", {
  m <- matrix(c(1.5, 2.5, 3.5, 4.5), nrow = 2)
  expect_no_warning(res <- resolveContingency(m, integerCounts = FALSE))
  expect_equal(res$n, 12)
})

test_that("resolveContingency stops if not square when square=TRUE", {
  m <- matrix(c(1,2,3,4,5,6), nrow = 2)
  expect_error(resolveContingency(m, square = TRUE), "square")
})

test_that("the returned components are named in camel case", {
  res <- resolveContingency(matrix(c(10, 5, 3, 8), nrow = 2))
  expect_named(res, c("table", "n", "r", "c", "dataName"))
})

# ── input forms ───────────────────────────────────────────────────────────────

test_that("a table object is used as it is", {
  x <- c("A","A","B","B")
  y <- c("X","Y","X","Y")
  tab <- table(x, y)
  res <- resolveContingency(tab)
  expect_equal(res$table, tab)
  expect_equal(res$n, 4L)
})

test_that("an xtabs object is accepted", {
  d <- data.frame(a = c("A","A","B","B"), b = c("X","Y","X","Y"),
                  n = c(3, 1, 2, 4))
  res <- resolveContingency(xtabs(n ~ a + b, data = d))
  expect_equal(res$r, 2L)
  expect_equal(res$n, 10)
})

test_that("a data frame of counts is coerced", {
  d <- data.frame(yes = c(10, 3), no = c(5, 12))
  res <- resolveContingency(d)
  expect_equal(res$r, 2L)
  expect_equal(res$c, 2L)
  expect_equal(res$n, 30)
  # the name is still taken from the expression, not from the coerced value
  expect_equal(res$dataName, "d")
})

test_that("an array with more than two dimensions is an error", {
  expect_error(resolveContingency(Titanic), "two-dimensional")
  a <- array(1:8, dim = c(2, 2, 2))
  expect_error(resolveContingency(a), "two-dimensional")
  # not even when a y of matching length is supplied
  expect_error(resolveContingency(a, rep(c("X","Y"), 4)), "two-dimensional")
})

test_that("y is required unless x is a table", {
  expect_error(resolveContingency(c("A","B")), "'y' must be given")
})

test_that("x and y must have the same length", {
  expect_error(resolveContingency(c("A","B","A"), c("X","Y")), "same length")
})

# ── dimensions ────────────────────────────────────────────────────────────────

test_that("a one-way table is rejected", {
  expect_error(resolveContingency(matrix(1:4, nrow = 1)),
               "at least two rows and columns")
  expect_error(resolveContingency(matrix(1:4, ncol = 1)),
               "at least two rows and columns")
  expect_error(resolveContingency(matrix(4, nrow = 1)),
               "at least two rows and columns")
})

test_that("each variable needs at least two levels", {
  expect_error(resolveContingency(c("A","A","A"), c("X","Y","X")),
               "at least 2 levels")
  expect_error(resolveContingency(c("A","B","A"), c("X","X","X")),
               "at least 2 levels")
})

test_that("square = TRUE accepts a square table", {
  m <- matrix(c(10, 5, 3, 8), nrow = 2)
  expect_no_error(resolveContingency(m, square = TRUE))
  # square only checks the dimensions, not that both axes carry the same
  # categories - that stays the caller's responsibility
  dimnames(m) <- list(c("A", "B"), c("yes", "no"))
  expect_no_error(resolveContingency(m, square = TRUE))
})

# ── counts ────────────────────────────────────────────────────────────────────

test_that("negative and non-finite counts are an error", {
  expect_error(resolveContingency(matrix(c(-1, 2, 3, 4), nrow = 2)),
               "nonnegative and finite")
  expect_error(resolveContingency(matrix(c(NA, 2, 3, 4), nrow = 2)),
               "nonnegative and finite")
  expect_error(resolveContingency(matrix(c(Inf, 2, 3, 4), nrow = 2)),
               "nonnegative and finite")
})

test_that("incomplete observations are dropped", {
  x <- c("A", "A", "B", "B", NA)
  y <- c("X", "Y", "X", NA, "Y")
  res <- resolveContingency(x, y)
  expect_equal(res$n, 3L)
})

test_that("levels emptied by the missing values are dropped", {
  x <- c("A", "A", "B", "B", "C")
  y <- c("X", "Y", "X", "Y", NA)
  res <- resolveContingency(x, y)
  expect_equal(res$r, 2L)
  expect_equal(rownames(res$table), c("A", "B"))
})

# ── argument validation ───────────────────────────────────────────────────────

test_that("square and integerCounts must be flags", {
  m <- matrix(c(10, 5, 3, 8), nrow = 2)
  expect_error(resolveContingency(m, square = NA), "square")
  expect_error(resolveContingency(m, square = c(TRUE, FALSE)), "square")
  expect_error(resolveContingency(m, square = 1), "square")
  expect_error(resolveContingency(m, integerCounts = NA), "integerCounts")
  expect_error(resolveContingency(m, integerCounts = "yes"), "integerCounts")
})

test_that("dataName must be a string or NULL", {
  m <- matrix(c(10, 5, 3, 8), nrow = 2)
  expect_error(resolveContingency(m, dataName = 1), "character string")
  expect_error(resolveContingency(m, dataName = c("a", "b")),
               "character string")
  expect_error(resolveContingency(m, dataName = NA_character_),
               "character string")
})

# ── dataName ──────────────────────────────────────────────────────────────────

test_that("dataName is derived from the arguments", {
  m <- matrix(c(10, 5, 3, 8), nrow = 2)
  expect_equal(resolveContingency(m)$dataName, "m")

  x <- c("A","A","B","B")
  y <- c("X","Y","X","Y")
  expect_equal(resolveContingency(x, y)$dataName, "x and y")
})

test_that("dataName can be supplied by the caller", {
  m <- matrix(c(10, 5, 3, 8), nrow = 2)
  expect_equal(resolveContingency(m, dataName = "smoking by sex")$dataName,
               "smoking by sex")

  # a wrapper reports the names seen at its own call site
  myTest <- function(a, b)
    resolveContingency(a, b,
                       dataName = paste(deparse1(substitute(a)), "and",
                                        deparse1(substitute(b))))$dataName
  x <- c("A","A","B","B")
  y <- c("X","Y","X","Y")
  expect_equal(myTest(x, y), "x and y")
})




test_that("resolveContingency validates matrix entries", {
  expect_error(
    resolveContingency(matrix(c(1, -1, 2, 3), nrow = 2)),
    "nonnegative and finite"
  )
  expect_error(
    resolveContingency(matrix(c(1, Inf, 2, 3), nrow = 2)),
    "nonnegative and finite"
  )
  expect_error(
    resolveContingency(matrix(c(1, NA, 2, 3), nrow = 2)),
    "nonnegative and finite"
  )
})

test_that("resolveContingency can allow non-integer counts", {
  m <- matrix(c(1.5, 2.5, 3.5, 4.5), nrow = 2)
  
  expect_no_warning(
    out <- resolveContingency(m, integerCounts = FALSE)
  )
  expect_equal(out$n, 12)
})

test_that("resolveContingency validates paired vectors", {
  expect_error(resolveContingency(c("A", "B")), "must be given")
  expect_error(
    resolveContingency(c("A", "B"), c("X")),
    "same length"
  )
  expect_error(
    resolveContingency(c("A", "A"), c("X", "Y")),
    "at least 2 levels"
  )
})

test_that("resolveContingency removes incomplete pairs", {
  x <- c("A", "A", "B", "B", NA)
  y <- c("X", "Y", "X", "Y", "X")
  
  out <- resolveContingency(x, y, dataName = "complete groups")
  
  expect_equal(out$n, 4)
  expect_identical(unname(rowSums(out$table)), c(2, 2))
  expect_identical(out$dataName, "complete groups")
})

test_that("resolveContingency handles square tables and data names", {
  m <- matrix(1:4, nrow = 2)
  out <- resolveContingency(m, square = TRUE, dataName = "custom table")
  
  expect_identical(out$r, 2L)
  expect_identical(out$c, 2L)
  expect_identical(out$dataName, "custom table")
  expect_error(
    resolveContingency(matrix(1, nrow = 1), square = TRUE),
    "at least two rows"
  )
})

test_that("resolveContingency derives default data names", {
  x <- c("A", "A", "B", "B")
  y <- c("X", "Y", "X", "Y")
  m <- matrix(1:4, nrow = 2)
  
  expect_identical(resolveContingency(x, y)$dataName, "x and y")
  expect_identical(resolveContingency(m)$dataName, "m")
})

