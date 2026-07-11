
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
#' distances.
#'
#' The returned logical value carries additional diagnostic information
#' as attributes:
#' \itemize{
#'   \item \code{eigenvalues}: Eigenvalues of the centered matrix
#'   \item \code{minEigenvalue}: Smallest eigenvalue
#'   \item \code{tol}: Tolerance used for the test
#' }
#'
#' @param distmat an object of class \code{dist}
#' @param tol numeric tolerance for detecting negative eigenvalues,
#'   relative to the largest absolute eigenvalue
#'
#' @return A logical scalar. Returns \code{TRUE} if the distance matrix is
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

  D <- as.matrix(distmat)
  n <- nrow(D)

  # double centering
  J <- diag(n) - 1 / n
  B <- -0.5 * J %*% (D^2) %*% J

  lambda <- eigen(B, symmetric = TRUE, only.values = TRUE)$values

  # relative tolerance: eigenvalues scale with the squared distances,
  # an absolute threshold would fail for rescaled point clouds
  res <- min(lambda) > -tol * max(abs(lambda), 1)

  # attach diagnostics
  attr(res, "eigenvalues") <- lambda
  attr(res, "minEigenvalue") <- min(lambda)
  attr(res, "tol") <- tol

  return(res)
}
