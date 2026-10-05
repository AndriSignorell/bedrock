
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
#' The argument `limits` allows full control over the limits. It can be:
#' \itemize{
#'   \item A numeric vector of length two specifying fixed bounds
#'   \item The result of a call to [quantile()] (e.g. with custom `type`)
#' }
#'
#' @param x a numeric vector to be winsorized.
#' @param limits a numeric vector of length two specifying the lower and upper
#'   winsorization limits. Defaults to the 5% and 95% quantiles of `x`
#'   with `na.rm = TRUE`.
#'
#' @return a numeric vector of the same length as `x`, where:
#' \itemize{
#'   \item values below the lower limit are replaced by the lower limit.
#'   \item values above the upper limit are replaced by the upper limit.
#'   \item missing values remain unchanged.
#' }
#'
#' @details
#' Winsorization is commonly used in robust statistics to reduce the influence
#' of outliers. In some cases, it can be beneficial to standardize the data
#' (e.g., using [scale()]) before applying winsorization.
#'
#' @examples
#' set.seed(9128)
#' x <- c(rnorm(10), NA, -100, 100)
#'
#' # Default winsorization (5% / 95% quantiles)
#' winsorize(x)
#'
#' # Winsorization using fixed bounds
#' winsorize(x, limits = c(-10, 10))
#'
#' # Custom quantile definition
#' winsorize(x, limits = quantile(x, c(0.1, 0.9), type = 1, na.rm = TRUE))
#'
#' # One-sided winsorization
#' winsorize(x, limits = c(-Inf, 2))  # upper bound only
#' winsorize(x, limits = c(-2, Inf)) # lower bound only
#'
#' @seealso `DescToolsX::scaleX()`, `robustHD::winsorize()`
#'
#' @family math.transform
#' @concept transformation
#' @concept outlier-detection
#' @export
winsorize <- function(
    x,
    limits = quantile(x, probs = c(0.05, 0.95), na.rm = TRUE)
) {

  if (!is.numeric(limits) || length(limits) != 2L || anyNA(limits))
    stop("'limits' must be a numeric vector of length 2 without NAs.")

  if (limits[1L] > limits[2L])
    stop("'limits[1]' must not exceed 'limits[2]'.")

  x[x < limits[1L]] <- limits[1L]
  x[x > limits[2L]] <- limits[2L]
  x
}

