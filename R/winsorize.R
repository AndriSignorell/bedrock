
#' Winsorize a Numeric Vector
#'
#' Winsorization replaces extreme values in a numeric vector by less extreme,
#' predefined bounds. Values below a lower limit are set to that limit, and
#' values above an upper limit are set to that upper limit.
#'
#' By default, the limits are defined as the 5% and 95% quantiles of the data.
#' Missing values are ignored when computing quantiles and are preserved in
#' the output.
#'
#' Formally, the winsorized vector \eqn{g(x)} is defined as:
#' \deqn{
#' g(x) =
#' \left\{
#' \begin{array}{ll}
#' l & \text{if } x \le l \\
#' x & \text{if } l < x < u \\
#' u & \text{if } x \ge u
#' \end{array}
#' \right.
#' }
#' where \eqn{l} and \eqn{u} denote the lower and upper bounds.
#'
#' The argument \code{val} allows full control over the limits. It can be:
#' \itemize{
#'   \item A numeric vector of length two specifying fixed bounds
#'   \item The result of a call to \code{\link{quantile}} (e.g. with custom \code{type})
#' }
#'
#' @param x A numeric vector to be winsorized.
#' @param val A numeric vector of length two specifying the lower and upper
#'   winsorization limits. Defaults to the 5% and 95% quantiles of \code{x}
#'   with \code{na.rm = TRUE}.
#'
#' @return A numeric vector of the same length as \code{x}, where:
#' \itemize{
#'   \item values below the lower limit are replaced by the lower limit
#'   \item values above the upper limit are replaced by the upper limit
#'   \item missing values remain unchanged
#' }
#'
#' @details
#' Winsorization is commonly used in robust statistics to reduce the influence
#' of outliers. In some cases, it can be beneficial to standardize the data
#' (e.g., using \code{\link{scale}}) before applying winsorization.
#'
#' @seealso \code{\link[DescToolsX]{scaleX}}, \code{\link[robustHD]{winsorize}}
#'
#' @examples
#' set.seed(9128)
#' x <- c(rnorm(10), NA, -100, 100)
#'
#' # Default winsorization (5% / 95% quantiles)
#' winsorize(x)
#'
#' # Winsorization using fixed bounds
#' winsorize(x, val = c(-10, 10))
#'
#' # Custom quantile definition
#' winsorize(x, val = quantile(x, c(0.1, 0.9), type = 1, na.rm = TRUE))
#'
#' # One-sided winsorization
#' winsorize(x, val = c(min(x, na.rm = TRUE), 2))  # upper bound only
#' winsorize(x, val = c(-2, max(x, na.rm = TRUE))) # lower bound only
#'
#' @family data_processing
#' @concept robust_statistics


#' @export
winsorize <- function(
    x,
    val = quantile(x, probs = c(0.05, 0.95), na.rm = TRUE)
) {
  x[x < val[1L]] <- val[1L]
  x[x > val[2L]] <- val[2L]
  x
}