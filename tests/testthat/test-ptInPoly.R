
# ── ptInPoly ──────────────────────────────────────────────────────────────────

test_that("ptInPoly identifies inside/outside points", {
  px <- c(0, 1, 1, 0)
  py <- c(0, 0, 1, 1)
  x  <- c(0.5, 1.5, 0.5)
  y  <- c(0.5, 0.5, 1.5)
  res <- ptInPoly(x, y, px, py)
  expect_equal(res[1], 1L)   # inside
  expect_equal(res[2], 0L)   # outside
  expect_equal(res[3], 0L)   # outside
})

test_that("ptInPoly treats boundary as inside", {
  px <- c(0, 1, 1, 0)
  py <- c(0, 0, 1, 1)
  res <- ptInPoly(0, 0, px, py)
  expect_equal(res, 1L)
})

test_that("ptInPoly stops on mismatched lengths", {
  expect_error(ptInPoly(c(1, 2), c(1), c(0,1,1,0), c(0,0,1,1)))
})

test_that("ptInPoly stops on polygon with < 3 vertices", {
  expect_error(ptInPoly(0.5, 0.5, c(0,1), c(0,1)))
})



