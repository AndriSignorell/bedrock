
#' Generalized Cross Product via Determinants
#'
#' Computes a vector orthogonal to all rows of a matrix using a determinant-based
#' construction. This generalizes the cross product to higher dimensions.
#'
#' For a matrix \eqn{A} with dimensions \eqn{n x (n+1)}, the result is a vector
#' in \eqn{R^{n+1}} orthogonal to all rows of \eqn{A}. The components are
#' given by:
#' \deqn{
#' v_i = (-1)^{i+1} \det(A_{-i})
#' }
#' where \eqn{A_{-i}} is the matrix obtained by removing the \eqn{i}-th column.
#'
#' For a vector of length 2, the function returns the perpendicular vector
#' \eqn{(a_2, -a_1)}, consistent with the formula above.
#'
#' Internally, the function computes a nullspace vector using SVD (which is
#' numerically stable also for ill-conditioned input) and rescales it to match
#' the magnitude of the determinant-based generalized cross product. For
#' numeric input, the sign is chosen to reproduce the orientation of the
#' determinant formula (and hence
#' anticommutativity: swapping two rows of \eqn{A} flips the sign of the
#' result). For complex input, where \code{det()} is not available, the sign
#' is fixed by the convention that the first component with non-zero modulus
#' has a positive real part.
#'
#' @param A A numeric or complex vector of length 2, or a matrix of dimension
#'   \eqn{n x (n+1)}.
#'
#' @return A numeric or complex vector of length \eqn{n+1}.
#'
#' @examples
#' # 2D case
#' crossProdN(c(1, 2))
#'
#' # 3D case (standard cross product)
#' A <- matrix(c(1,0,0,
#'               0,1,0), nrow = 2, byrow = TRUE)
#' crossProdN(A)
#'
#' # swapping rows flips the sign (anticommutativity)
#' crossProdN(A[2:1, ])
#'
#' @family math.utils
#' @concept numerical-methods
#' @export
crossProdN <- function(A) {
  if (!(is.numeric(A) || is.complex(A))) {
    stop("Argument 'A' must be numeric or complex.")
  }

  # reject arrays
  if (is.array(A) && length(dim(A)) > 2) {
    stop("Input must be a vector or 2D matrix.")
  }

  # vector case: v = (det(a2), -det(a1)) = (a2, -a1)
  if (is.null(dim(A))) {
    if (length(A) != 2L) {
      stop("Vector input must have length 2.")
    }
    return(c(A[2], -A[1]))
  }

  if (!is.matrix(A)) {
    stop("Input must be a vector or matrix.")
  }

  n <- nrow(A)
  m <- ncol(A)

  if (m != n + 1L) {
    stop("Matrix must be n x (n+1).")
  }

  # --- 1. direction via SVD ---
  sv <- svd(A, nu = 0, nv = m)
  v <- sv$v[, m]

  # --- 2. scaling ---
  scale <- prod(sv$d)
  v <- v * scale

  # --- 3. fix sign / orientation ---
  if (is.complex(v)) {
    # det() does not support complex matrices; fall back to the
    # convention that the first non-zero component has positive real part
    idx <- which(Mod(v) > .Machine$double.eps)[1]
    if (!is.na(idx) && Re(v[idx]) < 0) {
      v <- -v
    }
  } else {
    # orient v to match the determinant-based definition
    # v_i = (-1)^(i+1) det(A_{-i}): cofactor expansion along the last
    # row gives det(rbind(A, v)) = (-1)^n * |v|^2, so the correctly
    # oriented v satisfies (-1)^n * det(rbind(A, v)) > 0
    d <- det(rbind(A, v)) * (-1)^n
    if (!is.na(d) && d < 0) {
      v <- -v
    }
  }

  v
}
