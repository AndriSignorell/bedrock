test_that("vector: standard basis cross products", {
  expect_equal(crossProd(c(1,0,0), c(0,1,0)), c(0,0,1))
  expect_equal(crossProd(c(0,1,0), c(0,0,1)), c(1,0,0))
  expect_equal(crossProd(c(0,0,1), c(1,0,0)), c(0,1,0))
})

test_that("vector: anticommutativity  x × y == -(y × x)", {
  x <- c(1,2,3)
  y <- c(4,5,6)
  expect_equal(crossProd(x, y), -crossProd(y, x))
})

test_that("vector: parallel vectors yield zero vector", {
  x <- c(1,2,3)
  expect_equal(crossProd(x, x),       c(0,0,0))
  expect_equal(crossProd(x, 2*x),     c(0,0,0))
  expect_equal(crossProd(x, -x),      c(0,0,0))
})

test_that("vector: result is orthogonal to both inputs", {
  x <- c(1,2,3)
  y <- c(4,5,6)
  r <- crossProd(x, y)
  expect_equal(sum(r * x), 0)
  expect_equal(sum(r * y), 0)
})

test_that("vector: known numerical result", {
  expect_equal(crossProd(c(1,2,3), c(4,5,6)), c(-3, 6, -3))
})

test_that("vector: complex inputs", {
  x <- c(1+0i, 0+1i, 0+0i)
  y <- c(0+0i, 1+0i, 0+1i)
  res <- crossProd(x, y)
  expect_true(is.complex(res))
  expect_length(res, 3)
})

test_that("vector: mixed numeric and complex coercion", {
  x <- c(1, 0, 0)
  y <- c(0+0i, 1+0i, 0+0i)
  res <- crossProd(x, y)
  expect_true(is.complex(res))
  expect_equal(Re(res), c(0, 0, 1))
})

# ── matrix / rows ────────────────────────────────────────────────────────────

test_that("matrix rows: two standard-basis pairs", {
  x <- matrix(c(1,0,0,
                0,1,0), ncol = 3, byrow = TRUE)
  y <- matrix(c(0,1,0,
                0,0,1), ncol = 3, byrow = TRUE)
  res <- crossProd(x, y, "rows")
  expect_equal(res[1,], c(x=0, y=0, z=1))
  expect_equal(res[2,], c(x=1, y=0, z=0))
})

test_that("matrix rows: result shape equals input shape", {
  x <- matrix(runif(12), nrow = 4, ncol = 3)
  y <- matrix(runif(12), nrow = 4, ncol = 3)
  res <- crossProd(x, y, "rows")
  expect_equal(dim(res), c(4L, 3L))
})

test_that("matrix rows: colnames are c('x','y','z')", {
  x <- matrix(1:6, nrow = 2, ncol = 3)
  y <- matrix(6:1, nrow = 2, ncol = 3)
  res <- crossProd(x, y, "rows")
  expect_equal(colnames(res), c("x", "y", "z"))
})

test_that("matrix rows: rownames propagated from x", {
  x <- matrix(1:6, nrow = 2, ncol = 3, dimnames = list(c("v1","v2"), NULL))
  y <- matrix(6:1, nrow = 2, ncol = 3)
  res <- crossProd(x, y, "rows")
  expect_equal(rownames(res), c("v1", "v2"))
})

test_that("matrix rows: each row consistent with vector case", {
  x <- matrix(c(1,2,3, 4,5,6), nrow = 2, byrow = TRUE)
  y <- matrix(c(7,8,9, 1,0,0), nrow = 2, byrow = TRUE)
  res <- crossProd(x, y, "rows")
  expect_equal(unname(res[1,]), crossProd(x[1,], y[1,]))
  expect_equal(unname(res[2,]), crossProd(x[2,], y[2,]))
})

# ── matrix / cols ────────────────────────────────────────────────────────────

test_that("matrix cols: two standard-basis pairs", {
  x <- matrix(c(1,0,0, 0,1,0), nrow = 3, ncol = 2)
  y <- matrix(c(0,1,0, 0,0,1), nrow = 3, ncol = 2)
  res <- crossProd(x, y, "cols")
  expect_equal(res[,1], c(x=0, y=0, z=1))
  expect_equal(res[,2], c(x=1, y=0, z=0))
})

test_that("matrix cols: result shape equals input shape", {
  x <- matrix(runif(9), nrow = 3, ncol = 3)
  y <- matrix(runif(9), nrow = 3, ncol = 3)
  res <- crossProd(x, y, "cols")
  expect_equal(dim(res), c(3L, 3L))
})

test_that("matrix cols: rownames are c('x','y','z')", {
  x <- matrix(1:6, nrow = 3, ncol = 2)
  y <- matrix(6:1, nrow = 3, ncol = 2)
  res <- crossProd(x, y, "cols")
  expect_equal(rownames(res), c("x", "y", "z"))
})

test_that("matrix cols: colnames propagated from x", {
  x <- matrix(1:6, nrow = 3, ncol = 2,
              dimnames = list(NULL, c("a","b")))
  y <- matrix(6:1, nrow = 3, ncol = 2)
  res <- crossProd(x, y, "cols")
  expect_equal(colnames(res), c("a", "b"))
})

test_that("matrix cols: each col consistent with vector case", {
  x <- matrix(c(1,2,3, 4,5,6), nrow = 3)
  y <- matrix(c(7,8,9, 1,0,0), nrow = 3)
  res <- crossProd(x, y, "cols")
  expect_equal(unname(res[,1]), crossProd(x[,1], y[,1]))
  expect_equal(unname(res[,2]), crossProd(x[,2], y[,2]))
})

# ── error conditions ──────────────────────────────────────────────────────────

test_that("error: non-numeric / non-complex input", {
  expect_error(crossProd("a", c(1,0,0)),      "numeric or complex")
  expect_error(crossProd(c(1,0,0), TRUE),      "numeric or complex")
  expect_error(crossProd(list(1,2,3), c(1,2,3)), "numeric or complex")
})

test_that("error: vector wrong length", {
  expect_error(crossProd(c(1,2),   c(1,0,0)), "length 3")
  expect_error(crossProd(c(1,0,0), c(1,2)),   "length 3")
  expect_error(crossProd(1:4,      1:4),       "length 3")
})

test_that("error: higher-dimensional array", {
  a <- array(1:24, dim = c(2,3,4))
  expect_error(crossProd(a, a), "higher-dimensional arrays")
})

test_that("error: mixed vector and matrix", {
  expect_error(
    crossProd(c(1,0,0), matrix(1:6, nrow=2, ncol=3)),
    "both be vectors or both matrices"
  )
})

test_that("error: matrices with different dimensions", {
  x <- matrix(1:6, nrow=2, ncol=3)
  y <- matrix(1:9, nrow=3, ncol=3)
  expect_error(crossProd(x, y, "rows"), "identical dimensions")
})

test_that("error: rows orientation but ncol != 3", {
  x <- matrix(1:8, nrow=2, ncol=4)
  y <- matrix(1:8, nrow=2, ncol=4)
  expect_error(crossProd(x, y, "rows"), "3 columns")
})

test_that("error: cols orientation but nrow != 3", {
  x <- matrix(1:8, nrow=2, ncol=4)
  y <- matrix(1:8, nrow=2, ncol=4)
  expect_error(crossProd(x, y, "cols"), "3 rows")
})

test_that("error: invalid orientation string", {
  expect_error(crossProd(c(1,0,0), c(0,1,0), orientation = "diagonal"))
})
