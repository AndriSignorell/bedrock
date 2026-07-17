
#' Calculate Divisors
#'
#' Calculate the proper divisors of positive natural numbers.
#'
#' Divisibility is a mathematical relationship between two integers. An integer
#' is divisible by another integer if there is no remainder in the division.
#' This function returns the \emph{proper} divisors of \code{x}, i.e. all
#' positive divisors excluding \code{x} itself. The number 11 is prime and has
#' only the proper divisor 1, whereas the number 12 has the proper divisors
#' 1, 2, 3, 4 and 6. In elementary number theory, the concept of divisibility
#' is limited to natural numbers. The number of proper divisors can be
#' determined with the function \code{\link{length}()}.
#'
#' @param x vector of positive whole numbers for which the divisors are to
#'   be returned.
#'
#' @return an integer vector containing the proper divisors if \code{x} is a
#'   single number, otherwise a named list of such vectors.
#'
#' @examples
#'
#' divisors(786)
#'
#' divisors(c(145, 786))
#'
#' @family number.theory
#' @concept number-theory
#' @concept numerical-methods
#' @export
divisors <- function(x) {

  if (!is.numeric(x) || anyNA(x) || any(x %% 1 != 0) || any(x < 1))
    stop("'x' must contain positive whole numbers only.")

  if (any(x > .Machine$integer.max))
    stop("'x' must not exceed .Machine$integer.max (", .Machine$integer.max, ").")

  res <- setNamesX(lapply(x, divs), x)

  if (length(x) == 1L)
    res[[1L]]
  else
    res
}
