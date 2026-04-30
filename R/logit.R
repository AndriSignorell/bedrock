
#' Logit Transformation and Its Inverse
#'
#' Computes the logit transformation and its inverse for values defined on a
#' finite interval \eqn{[min, max]}.
#'
#' The \code{logit()} function maps values from \eqn{[min, max]} to the real
#' line \eqn{(-\infty, \infty)}. The inverse transformation \code{logitInv()}
#' maps real-valued inputs back to \eqn{[min, max]}.
#'
#' @param x Numeric vector. For \code{logit()}, values are interpreted relative
#'   to the interval \eqn{[min, max]}. For \code{logitInv()}, \code{x} can be any
#'   real number.
#' @param min Lower bound of the interval. Must be finite.
#' @param max Upper bound of the interval. Must be finite and greater than \code{min}.
#' @param eps Small positive value used to clamp probabilities away from
#'   \eqn{0} and \eqn{1} for numerical stability in \code{logit()}.
#'   Default: \code{.Machine$double.eps}.
#' @param warn Logical; if \code{TRUE}, a warning is issued when values are
#'   effectively clamped because they fall outside the open interval
#'   \eqn{(min, max)}. Default: \code{FALSE}.
#'
#' @return A numeric vector of the same length as \code{x}.
#'
#' @details
#' The logit transformation is defined as:
#'
#' \deqn{
#' \mathrm{logit}(x) = \log\left(\frac{p}{1 - p}\right)
#' }
#'
#' where
#'
#' \deqn{
#' p = \frac{x - min}{max - min}.
#' }
#'
#' For numerical stability, \eqn{p} is clamped to \eqn{[eps, 1 - eps]} before
#' applying the transformation. This prevents returning \code{-Inf} or
#' \code{Inf} for values exactly equal to \code{min} or \code{max}, or slightly
#' outside the interval due to floating point error.
#'
#' If \code{warn = TRUE}, a warning is issued when such clamping occurs.
#'
#' The inverse transformation is given by:
#'
#' \deqn{
#' x = min + (max - min) \cdot \frac{1}{1 + e^{-z}}
#' }
#'
#' where \eqn{z} is the input to \code{logitInv()}.
#'
#' Note that \code{logitInv()} does not perform clamping. This asymmetry is
#' intentional: \code{\link[stats]{plogis}} is well-defined for all real inputs,
#' so no stabilization is required.
#'

#' @seealso \code{\link[stats]{qlogis}}, \code{\link[stats]{plogis}}
#'
#' @family transformations
#' @concept transformation
#' @concept logistic
#' 
#' @examples
#' x <- seq(0, 1, length.out = 5)
#' z <- logit(x)
#' logitInv(z)
#'
#' # Boundary values are clamped internally:
#' # 0 -> eps, 1 -> 1 - eps
#' logit(c(0, 0.5, 1))
#'
#' # With warn = TRUE, clamping at the boundaries triggers a warning
#' logit(c(0, 0.5, 1), warn = TRUE)
#'
#' # Values strictly outside the interval also trigger a warning
#' logit(c(-0.1, 0.5, 1.1), warn = TRUE)
#'
#' # Custom interval
#' x <- seq(10, 20, length.out = 5)
#' z <- logit(x, min = 10, max = 20)
#' logitInv(z, min = 10, max = 20)



#' @rdname logit
#' @export
logit <- function(x, min = 0, max = 1,
                  eps = .Machine$double.eps,
                  warn = FALSE) {
  
  if (!is.numeric(x))
    stop("x must be numeric")
  
  if (!is.finite(min) || !is.finite(max) || min >= max)
    stop("min < max must hold")
  
  p <- (x - min) / (max - min)
  
  if (warn && any(p <= 0 | p >= 1, na.rm = TRUE)) {
    warning("Values outside (min, max) were clamped to avoid -Inf/Inf")
  }
  
  p <- pmin(pmax(p, eps), 1 - eps)
  
  qlogis(p)
}


#' @rdname logit
#' @export
logitInv <- function(x, min = 0, max = 1) {
  
  if (!is.numeric(x))
    stop("x must be numeric")
  
  if (!is.finite(min) || !is.finite(max) || min >= max)
    stop("min < max must hold")
  
  p <- plogis(x)
  
  p * (max - min) + min
}

