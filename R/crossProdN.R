
#' Generalized Cross Product via Determinants
#'
#' Computes a vector orthogonal to all rows of a matrix using a determinant-based
#' construction. This generalizes the cross product to higher dimensions.
#'
#' @param A A numeric or complex vector of length 2, or a matrix of dimension
#'   \eqn{n \times (n+1)}.
#'
#' @details
#' For a matrix \eqn{A} with dimensions \eqn{n \times (n+1)}, the result is a vector
#' in \eqn{\mathbb{R}^{n+1}} orthogonal to all rows of \eqn{A}. The components are
#' given by:
#' \deqn{
#' v_i = (-1)^{i+1} \det(A_{-i})
#' }
#' where \eqn{A_{-i}} is the matrix obtained by removing the \eqn{i}-th column.
#'
#' For a vector of length 2, the function returns a perpendicular vector.
#'
#' This function computes a nullspace vector using SVD and rescales it to match
#' the magnitude of the determinant-based generalized cross product.
#' The sign is fixed by enforcing the first non-zero component to be positive.
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



#' @export
crossProdN <- function(A) {
  if (!(is.numeric(A) || is.complex(A))) {
    stop("Argument 'A' must be numeric or complex.")
  }
  
  # reject arrays
  if (is.array(A) && length(dim(A)) > 2) {
    stop("Input must be a vector or 2D matrix.")
  }
  
  # vector case
  if (is.null(dim(A))) {
    if (length(A) != 2L) {
      stop("Vector input must have length 2.")
    }
    return(c(-A[2], A[1]))  # deterministic!
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
  sv <- svd(A)
  v <- sv$v[, m]
  
  # --- 2. scaling  ---
  scale <- prod(sv$d)
  v <- v * scale
  
  # --- 3. fix sign ---
  # Convention: first non-null element is positive
  idx <- which(Mod(v) > .Machine$double.eps)[1]
  if (!is.na(idx) && Re(v[idx]) < 0) {
    v <- -v
  }

  v
}
