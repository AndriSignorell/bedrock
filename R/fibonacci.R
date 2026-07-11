
#' Fibonacci Numbers
#'
#' Generate Fibonacci numbers. The Fibonacci numbers can also be calculated
#' using the golden ratio \code{phi}, as demonstrated in the examples.
#'
#' Generates the \code{n}-th Fibonacci number, whereas \code{fibonacci(0) = 0}.
#' \cr The golden ratio is defined as \code{phi = 0.5*(1+sqrt(5))}.
#'
#' Values of \code{n} are limited to 78, as larger Fibonacci numbers exceed
#' the range in which doubles represent integers exactly (\code{2^53}).
#'
#' @param n nonnegative integer (<= 78) or vector of such integers
#' @return An integer-valued numeric vector.
#'
#' @references \url{https://en.wikipedia.org/wiki/Fibonacci_number}\cr
#' \url{https://mathworld.wolfram.com/GoldenRatio.html}
#'
#' @examples
#'
#' fibonacci(0)                            # 0
#' fibonacci(2)                            # 1
#' fibonacci(0:3)                          # 0 1 1 2
#' fibonacci(0:25)                         # ... 75025 121393
#'
#' # Golden ratio = Fib(25)/ Fib(24)
#' f25 <- quot(fibonacci(24:25))           # 1.618033989
#' phi <- (sqrt(5) + 1)/2
#' abs(f25 - phi)                          # 7.945178e-11
#'
#' # Fibonacci numbers without iteration
#' fibo <- function(n) {
#'   phi <- (sqrt(5) + 1)/2
#'   fib <- (phi^(n+1) - (1-phi)^(n+1)) / (2*phi - 1)
#'   round(fib)
#' }
#'
#' fibo(30:33)                             # 1346269 2178309 3524578 5702887
#'
#' @family number.theory
#' @concept number-theory
#' @concept numerical-methods
#' @export
fibonacci <- function(n) {

  if (!is.numeric(n) || anyNA(n) || any(!isWholeLike(n)) || any(n < 0))
    stop("Argument 'n' must be an integer >= 0.")

  # F(79) exceeds 2^53 and could no longer be represented exactly
  if (any(n > 78))
    stop("Argument 'n' must be <= 78 (larger Fibonacci numbers exceed exact double precision).")

  maxn <- max(n)
  if (maxn <= 2L)
    return(c(0, 1, 1)[n + 1L])

  # compute in double: integer arithmetic would overflow at F(47)
  z <- c(0, 1, 1, rep(NA_real_, maxn - 2L))
  for (i in 4L:(maxn + 1L)) {
    z[i] <- z[i - 1L] + z[i - 2L]
  }

  z[n + 1L]

}
