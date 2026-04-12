
#' Dot Product of Vectors or Matrices
#'
#' Computes the dot product between two numeric or complex vectors or matrices.
#' Internally uses \code{\link[base]{crossprod}} for efficient computation.
#'
#' @param x A numeric or complex vector, or a numeric/complex matrix.
#' @param y A numeric or complex vector, or a numeric/complex matrix with the same dimensions as \code{x}.
#'
#' @details
#' For vectors \eqn{x} and \eqn{y}, the dot product is defined as:
#' \deqn{
#' \sum_i \overline{x_i} y_i
#' }
#' where \eqn{\overline{x_i}} denotes the complex conjugate of \eqn{x_i}.
#'
#' For matrices, the dot product is computed column-wise, corresponding to:
#' \deqn{
#' X^H Y
#' }
#' where \eqn{X^H} is the conjugate transpose of \eqn{X}.
#'
#' The implementation relies on \code{\link[base]{crossprod}}, which is typically
#' optimized via BLAS for high performance.
#'
#' @return
#' \itemize{
#'   \item A scalar if \code{x} and \code{y} are vectors.
#'   \item A numeric or complex vector containing column-wise dotProd products if matrices are supplied.
#' }
#'
#' @examples
#' # Vector dot product
#' dotProd(c(1, 2, 3), c(4, 5, 6))
#'
#' # Complex vectors
#' dotProd(c(1+1i, 2), c(3, 4-1i))
#'
#' # Matrix (column-wise dot products)
#' x <- matrix(1:6, ncol = 2)
#' y <- matrix(6:1, ncol = 2)
#' dotProd(x, y)
#'
#' @seealso \code{\link[base]{crossprod}}
#'



#' @export
dotProd <- function(x, y) {
  if (!(is.numeric(x) || is.complex(x)) ||
      !(is.numeric(y) || is.complex(y))) {
    stop("Arguments must be real or complex.")
  }
  
  # vector case
  if (is.null(dim(x)) && is.null(dim(y))) {
    if (length(x) != length(y)) {
      stop("Vectors must have same length.")
    }
    return(drop(crossprod(x, y)))
  }
  
  # matrix case
  if (!identical(dim(x), dim(y))) {
    stop("Matrices must have same dimensions.")
  }
  
  drop(crossprod(x, y))
}



