
#' Calculate Divisors
#'
#' Calculate divisors of positive natural numbers.
#'
#' Divisibility is a mathematical relationship between two integers. An integer
#' is divisible by another integer if there is no remainder in the division.
#' The number 11 has only two divisors: 1 and the number 11 itself, whereas the
#' number 12 has many divisors: 1, 2, 3, 4, 6 and 12.  In elementary number
#' theory, the concept of divisibility is limited to natural numbers.  The
#' number of its divisors can be determined with the function
#' \code{\link{length}()}.
#'
#' @param x vector of positive whole numbers for which the divisors are to
#'   be returned
#'
#' @return an integer vector containing the divisors if \code{x} is a single
#'   number, otherwise a named list of such vectors
#'
#' @author Andri Signorell <andri@@signorell.net>
#'
#' @examples
#'
#' divisors(786)
#'
#' divisors(c(145, 786))
#'
#' @family number.theory
#' @concept number-theory
#' @export
divisors <- function(x) {

  if (!is.numeric(x) || anyNA(x) || any(x %% 1 != 0) || any(x < 1))
    stop("'x' must contain positive whole numbers only.")

  res <- setNamesX(lapply(x, divs), x)

  if (length(x) == 1L)
    res[[1L]]
  else
    res
}
