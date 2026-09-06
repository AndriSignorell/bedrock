library(testthat)

test_that("isEuclid identifies Euclidean distances", {

  d <- dist(matrix(rnorm(20), ncol=2))

  expect_true(isEuclid(d))
})

test_that("isEuclid returns attributes", {

  d <- dist(matrix(rnorm(20), ncol=2))
  res <- isEuclid(d)

  expect_true(!is.null(attr(res, "eigenvalues")))
  expect_true(!is.null(attr(res, "minEigenvalue")))
})

test_that("isEuclid errors on invalid input", {

  expect_error(isEuclid(matrix(1:4,2)))
})



test_that("isEuclid detects non-Euclidean distances at different scales", {
  D <- matrix(
    c(
      0, 1, 1,
      1, 0, 3,
      1, 3, 0
    ),
    nrow = 3,
    byrow = TRUE
  )
  
  expect_false(isEuclid(as.dist(D)))
  expect_false(isEuclid(as.dist(D * 1e-8)))
  expect_false(isEuclid(as.dist(D * 1e8)))
})

test_that("isEuclid handles zero distances and reports diagnostics", {
  out <- isEuclid(as.dist(matrix(0, nrow = 3, ncol = 3)), tol = 1e-8)
  
  expect_true(out)
  expect_length(attr(out, "eigenvalues"), 3L)
  expect_identical(
    attr(out, "minEigenvalue"),
    min(attr(out, "eigenvalues"))
  )
  expect_identical(attr(out, "tol"), 1e-8)
})

test_that("isEuclid validates its tolerance", {
  d <- dist(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE))
  
  for (tol in list(-1, NA_real_, Inf, c(0, 1), "0.1")) {
    expect_error(
      isEuclid(d, tol = tol),
      "single non-negative number",
      info = paste("tol =", paste(tol, collapse = ", "))
    )
  }
})

test_that("isEuclid rejects undersized and missing distance matrices", {
  expect_error(
    isEuclid(as.dist(matrix(0, nrow = 1, ncol = 1))),
    "at least two"
  )
  
  D <- matrix(c(0, NA, NA, 0), nrow = 2)
  expect_error(isEuclid(as.dist(D)), "missing values")
})

