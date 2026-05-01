
#' Cross Product of 3D Vectors or Matrices
#'
#' Computes the cross product in three-dimensional space for vectors or matrices.
#' For matrices, the operation can be applied row-wise or column-wise.
#'
#' @param x A numeric or complex vector of length 3, or a matrix with one dimension of length 3.
#' @param y A numeric or complex vector or matrix with the same dimensions as \code{x}.
#' @param orientation Character string specifying whether vectors are stored in rows or columns
#'   when matrices are supplied. Must be one of \code{"rows"} or \code{"cols"}.
#'   Ignored if \code{x} and \code{y} are vectors.
#'
#' @details
#' For vectors \eqn{x = (x_1, x_2, x_3)} and \eqn{y = (y_1, y_2, y_3)}, the cross product is:
#' \deqn{
#' x \times y = (x_2 y_3 - x_3 y_2,\; x_3 y_1 - x_1 y_3,\; x_1 y_2 - x_2 y_1)
#' }
#'
#' For matrix inputs:
#' \itemize{
#'   \item \code{orientation = "rows"}: each row is treated as a vector (requires \code{ncol(x) == 3})
#'   \item \code{orientation = "cols"}: each column is treated as a vector (requires \code{nrow(x) == 3})
#' }
#'
#' Numeric and complex inputs can be mixed; standard R coercion rules apply.
#'
#' @return
#' \itemize{
#'   \item A vector of length 3 if inputs are vectors.
#'   \item A matrix with propagated dimension names if matrices are supplied.
#' }
#'
#' @examples
#' # Vector case
#' crossProd(c(1,0,0), c(0,1,0))
#'
#' # Row-wise
#' x <- matrix(c(1,0,0,
#'               0,1,0), ncol = 3, byrow = TRUE)
#' y <- matrix(c(0,1,0,
#'               0,0,1), ncol = 3, byrow = TRUE)
#' crossProd(x, y, "rows")
#'
#' # Column-wise
#' x <- matrix(1:9, nrow = 3)
#' y <- matrix(9:1, nrow = 3)
#' crossProd(x, y, "cols")
#'


#' @family math.utils
#' @concept mathematics
#' @concept vector-manipulation
#'
#'
#' @export
crossProd <- function(x, y, orientation = c("rows", "cols")) {
  orientation <- match.arg(orientation)
  
  # type check
  if (!(is.numeric(x) || is.complex(x)) ||
      !(is.numeric(y) || is.complex(y))) {
    stop("Arguments must be numeric or complex.")
  }
  
  # reject higher-dimensional arrays
  if ((is.array(x) && length(dim(x)) > 2) ||
      (is.array(y) && length(dim(y)) > 2)) {
    stop("Inputs must be vectors or 2D matrices (no higher-dimensional arrays).")
  }
  
  # vector case
  if (is.null(dim(x)) && is.null(dim(y))) {
    if (length(x) != 3L || length(y) != 3L) {
      stop("Vectors must have length 3.")
    }
    return(c(
      x[2]*y[3] - x[3]*y[2],
      x[3]*y[1] - x[1]*y[3],
      x[1]*y[2] - x[2]*y[1]
    ))
  }
  
  # matrix case
  if (!is.matrix(x) || !is.matrix(y)) {
    stop("Inputs must both be vectors or both matrices.")
  }
  
  if (!identical(dim(x), dim(y))) {
    stop("Matrices must have identical dimensions.")
  }
  
  if (orientation == "rows") {
    if (ncol(x) != 3L) {
      stop("For orientation = 'rows', matrices must have 3 columns.")
    }
    
    res <- cbind(
      x[,2]*y[,3] - x[,3]*y[,2],
      x[,3]*y[,1] - x[,1]*y[,3],
      x[,1]*y[,2] - x[,2]*y[,1]
    )
    
    # names
    rownames(res) <- rownames(x)
    colnames(res) <- c("x", "y", "z")
    
  } else {
    if (nrow(x) != 3L) {
      stop("For orientation = 'cols', matrices must have 3 rows.")
    }
    
    res <- rbind(
      x[2,]*y[3,] - x[3,]*y[2,],
      x[3,]*y[1,] - x[1,]*y[3,],
      x[1,]*y[2,] - x[2,]*y[1,]
    )
    
    # names (symmetric!)
    colnames(res) <- colnames(x)
    rownames(res) <- c("x", "y", "z")
  }
  
  res
}

