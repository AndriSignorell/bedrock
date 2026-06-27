

#' Point-in-Polygon Test (Angle Summation, Rcpp)
#'
#' Determines whether points lie inside a polygon.
#' Points located exactly on polygon edges or vertices are treated as **inside**.
#'
#' The function uses a numerically stable angle summation algorithm
#' implemented in C++ via Rcpp.
#'
#' @param x Numeric vector of x-coordinates of the query points.
#' @param y Numeric vector of y-coordinates of the query points.
#' @param polyX Numeric vector of x-coordinates of the polygon vertices.
#' @param polyY Numeric vector of y-coordinates of the polygon vertices.
#'
#' @return An integer vector of length `length(x)`:
#' \describe{
#'   \item{0}{point is outside the polygon}
#'   \item{1}{point is inside the polygon or on its boundary}
#' }
#'
#' @details
#' - The polygon is implicitly closed (last vertex connects to the first).
#' - The polygon is assumed to be non-self-intersecting.
#' - Numerical robustness is ensured via epsilon-based comparisons.
#'
#' @examples
#' # Define a square
#' px <- c(0, 1, 1, 0)
#' py <- c(0, 0, 1, 1)
#'
#' # Query points
#' x <- c(0.5, 1.5, 0, 0.5)
#' y <- c(0.5, 0.5, 0, 1)
#'
#' ptInPoly(x, y, px, py)
#'

#' @family math.utils
#' @concept mathematics
#' @concept geometry
#'
#'
#' @export
ptInPoly <- function(x, y, polyX, polyY) {
  
  # Input checks
  if (length(x) != length(y)) {
    stop("x and y must have the same length")
  }
  
  if (length(polyX) != length(polyY)) {
    stop("polyX and polyY must have the same length")
  }
  
  if (length(polyX) < 3) {
    stop("Polygon must have at least 3 vertices")
  }
  
  # Ensure numeric type
  x <- as.numeric(x)
  y <- as.numeric(y)
  polyX <- as.numeric(polyX)
  polyY <- as.numeric(polyY)
  
  # Call C++ implementation
  pip_cpp(x, y, polyX, polyY)
}

