
#' Test if a Distance Matrix Is Euclidean
#'
#' Checks whether a distance matrix corresponds to Euclidean distances.
#'
#' The test is based on the eigenvalues of the double-centered squared
#' distance matrix \eqn{B = -\frac{1}{2} J D^2 J}. A distance matrix is
#' Euclidean if and only if \eqn{B} is positive semi-definite, i.e., all
#' eigenvalues are non-negative (within numerical tolerance).
#'
#' The tolerance is applied \emph{relative} to the largest absolute
#' eigenvalue, so that the test is invariant to rescaling of the
#' distances. Note that this holds in both directions: the comparison
#' below uses \code{max(abs(lambda))} without an absolute floor, so
#' shrinking all distances by a constant factor cannot turn a
#' non-Euclidean matrix into a Euclidean one.
#'
#' The returned logical value carries additional diagnostic information
#' as attributes:
#' \itemize{
#'   \item \code{eigenvalues}: Eigenvalues of the centered matrix
#'   \item \code{minEigenvalue}: Smallest eigenvalue
#'   \item \code{tol}: Tolerance used for the test
#' }
#'
#' @param distmat an object of class \code{dist}.
#' @param tol numeric tolerance for detecting negative eigenvalues,
#'   relative to the largest absolute eigenvalue.
#'
#' @return a logical scalar. Returns \code{TRUE} if the distance matrix is
#'   (approximately) Euclidean, otherwise \code{FALSE}.
#'
#' @examples
#' d <- dist(matrix(rnorm(20), ncol = 2))
#' res <- isEuclid(d)
#' res
#'
#' # Access diagnostics
#' attr(res, "eigenvalues")
#' attr(res, "minEigenvalue")
#'
#' @family data.predicate
#' @concept type-test
#' @concept geometry
#' @export
isEuclid <- function(distmat, tol = 1e-07) {
  if (!inherits(distmat, "dist"))
    stop("Object of class 'dist' expected")

  if (!is.numeric(tol) || length(tol) != 1L || !is.finite(tol) || tol < 0)
    stop("'tol' must be a single non-negative number")

  D <- as.matrix(distmat)
  n <- nrow(D)

  if (n < 2L)
    stop("'distmat' must contain at least two objects")

  if (anyNA(D))
    stop("'distmat' must not contain missing values")

  # double centering
  J <- diag(n) - 1 / n
  B <- -0.5 * J %*% (D^2) %*% J

  lambda <- eigen(B, symmetric = TRUE, only.values = TRUE)$values

  # relative tolerance: eigenvalues scale with the squared distances,
  # an absolute threshold would fail for rescaled point clouds
  # No floor of 1 on the scale. `max(abs(lambda), 1)` made the threshold
  # an ABSOLUTE -tol whenever every eigenvalue was below 1 in magnitude,
  # which is exactly the case for a point cloud scaled down - so the
  # invariance promised above held for enlarging the distances but not
  # for shrinking them, and a non-Euclidean matrix could be scaled into
  # passing. An all-zero distance matrix is Euclidean by definition and
  # is caught separately.
  scale <- max(abs(lambda))

  res <- if (scale == 0) TRUE else min(lambda) > -tol * scale

  # attach diagnostics
  attr(res, "eigenvalues") <- lambda
  attr(res, "minEigenvalue") <- min(lambda)
  attr(res, "tol") <- tol

  return(res)
}

