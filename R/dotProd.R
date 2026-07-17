
#' Dot Product of Vectors or Matrices
#'
#' Computes the dot product between two numeric or complex vectors, or the
#' column-wise dot products of two matrices with identical dimensions.
#'
#' For vectors \eqn{x} and \eqn{y}, the dot product is defined as:
#' \deqn{
#' \sum_i \overline{x_i} y_i
#' }
#' where \eqn{\overline{x_i}} denotes the complex conjugate of \eqn{x_i}
#' (for real input this is simply \eqn{\sum_i x_i y_i}).
#'
#' For matrices, the dot product of each column of \code{x} with the
#' corresponding column of \code{y} is returned.
#'
#' Note that \code{\link[base]{crossprod}} does \emph{not} conjugate its
#' first argument for complex input, so it computes \eqn{t(X) Y} rather
#' than the Hermitian inner product; this function does conjugate.
#'
#' @param x a numeric or complex vector, or a numeric/complex matrix.
#' @param y a numeric or complex vector, or a numeric/complex matrix with the
#'   same dimensions as \code{x}.
#'
#' @return
#' \itemize{
#'   \item a scalar if \code{x} and \code{y} are vectors.
#'   \item a numeric or complex vector containing the column-wise dot
#'   products if matrices are supplied.
#' }
#'
#' @examples
#' # Vector dot product
#' dotProd(c(1, 2, 3), c(4, 5, 6))
#'
#' # Complex vectors (Hermitian inner product)
#' dotProd(c(1+1i, 2), c(3, 4-1i))
#'
#' # Matrix (column-wise dot products)
#' x <- matrix(1:6, ncol = 2)
#' y <- matrix(6:1, ncol = 2)
#' dotProd(x, y)
#'
#' @seealso \code{\link[base]{crossprod}}
#'
#' @family math.basic
#' @concept linear-algebra
#' @concept numerical-methods
#' @export
dotProd <- function(x, y) {
  if (!(is.numeric(x) || is.complex(x)) ||
      !(is.numeric(y) || is.complex(y))) {
    stop("Arguments must be real or complex.")
  }

  if (is.complex(x)) {
    x <- Conj(x)
  }

  # vector case
  if (is.null(dim(x)) && is.null(dim(y))) {
    if (length(x) != length(y)) {
      stop("Vectors must have same length.")
    }
    return(sum(x * y))
  }

  # matrix case
  if (!identical(dim(x), dim(y))) {
    stop("Matrices must have same dimensions.")
  }

  colSums(x * y)
}
