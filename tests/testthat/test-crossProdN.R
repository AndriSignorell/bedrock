test_that("2D vector: perpendicular to input", {
  v <- crossProdN(c(1, 2))
  expect_equal(sum(v * c(1, 2)), 0)
})

test_that("2D vector: correct formula c(-b, a)", {
  expect_equal(crossProdN(c(1, 2)),  c(-2,  1))
  expect_equal(crossProdN(c(3, 0)),  c( 0,  3))
  expect_equal(crossProdN(c(0, 1)),  c(-1,  0))
  expect_equal(crossProdN(c(-1, 4)), c(-4, -1))
})

test_that("2D vector: length equals input length", {
  x <- c(3, 4)
  expect_equal(sqrt(sum(crossProdN(x)^2)), sqrt(sum(x^2)))
})

# ── 3D case (n=2 matrix, output length 3) ────────────────────────────────────

test_that("3D case: result orthogonal to both rows", {
  A <- matrix(c(1,0,0,
                0,1,0), nrow = 2, byrow = TRUE)
  v <- crossProdN(A)
  expect_equal(as.numeric(A %*% v), c(0, 0))
})

test_that("3D case: agrees with cross3d on standard basis", {
  A <- matrix(c(1,0,0,
                0,1,0), nrow = 2, byrow = TRUE)
  v <- crossProdN(A)
  expected <- cross3d(c(1,0,0), c(0,1,0))
  expect_equal(unname(v), unname(expected))
})

test_that("3D case: agrees with cross3d on arbitrary vectors", {
  x <- c(1, 2, 3)
  y <- c(4, 5, 6)
  A <- matrix(c(x, y), nrow = 2, byrow = TRUE)
  v <- crossProdN(A)
  expected <- cross3d(x, y)
  expect_equal(unname(v), unname(expected))
})

test_that("3D case: correct norm (= area of parallelogram)", {
  x <- c(2, 0, 0)
  y <- c(0, 3, 0)
  A <- matrix(c(x, y), nrow = 2, byrow = TRUE)
  v <- crossProdN(A)
  expect_equal(sqrt(sum(v^2)), 6)  # 2 * 3
})

# ── 4D case (n=3 matrix, output length 4) ────────────────────────────────────

test_that("4D case: result orthogonal to all rows", {
  set.seed(42)
  A <- matrix(rnorm(12), nrow = 3, ncol = 4)
  v <- crossProdN(A)
  expect_equal(as.numeric(A %*% v), rep(0, 3), tolerance = 1e-10)
})

test_that("4D case: result has length 4", {
  A <- matrix(1:12, nrow = 3, ncol = 4)
  v <- crossProdN(A)
  expect_length(v, 4)
})

test_that("4D case: correct norm", {
  # orthonormal rows -> volume = 1 -> norm = 1
  A <- matrix(c(1,0,0,0,
                0,1,0,0,
                0,0,1,0), nrow = 3, byrow = TRUE)
  v <- crossProdN(A)
  expect_equal(sqrt(sum(v^2)), 1, tolerance = 1e-10)
})

# ── sign convention ───────────────────────────────────────────────────────────

test_that("sign: first non-zero component is positive", {
  set.seed(7)
  for (i in 1:10) {
    A <- matrix(rnorm(6), nrow = 2, ncol = 3)
    v <- crossProdN(A)
    idx <- which(abs(v) > .Machine$double.eps)[1]
    if (!is.na(idx)) expect_gt(Re(v[idx]), 0)
  }
})

test_that("sign: deterministic across repeated calls", {
  A <- matrix(c(1,2,3, 4,5,6), nrow = 2, byrow = TRUE)
  expect_equal(crossProdN(A), crossProdN(A))
})

# ── complex inputs ────────────────────────────────────────────────────────────

test_that("complex: 2D vector returns complex perpendicular", {
  z <- c(1+1i, 2+0i)
  v <- crossProdN(z)
  expect_true(is.complex(v))
  expect_equal(sum(Re(Conj(v) * z)), 0, tolerance = 1e-10)
})

test_that("complex: matrix result orthogonal to rows", {
  A <- matrix(c(1+1i, 0+0i, 0+0i,
                0+0i, 1+0i, 0+1i), nrow = 2, byrow = TRUE)
  v <- crossProdN(A)
  residuals <- Mod(A %*% v)
  expect_true(all(residuals < 1e-10))
})

# ── error conditions ──────────────────────────────────────────────────────────

test_that("error: non-numeric input", {
  expect_error(crossProdN("a"),          "numeric or complex")
  expect_error(crossProdN(list(1, 2)),   "numeric or complex")
  expect_error(crossProdN(TRUE),         "numeric or complex")
})

test_that("error: vector wrong length", {
  expect_error(crossProdN(c(1, 2, 3)),   "length 2")
  expect_error(crossProdN(1),            "length 2")
})

test_that("error: higher-dimensional array", {
  a <- array(1:24, dim = c(2, 3, 4))
  expect_error(crossProdN(a), "2D matrix")
})

test_that("error: matrix wrong dimensions (not n x n+1)", {
  expect_error(crossProdN(matrix(1:6, nrow = 2, ncol = 3) |>
                            (\(m) m[, 1:2])()),  "n x \\(n\\+1\\)")
  expect_error(crossProdN(matrix(1:9, nrow = 3, ncol = 3)), "n x \\(n\\+1\\)")
})

test_that("error: non-matrix non-vector (data.frame)", {
  expect_error(crossProdN(data.frame(x=1:3, y=1:3, z=1:3)), "numeric or complex")
})
