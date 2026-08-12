
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
#' @return an integer vector containing the proper divisors in ascending
#'   order if \code{x} is a single number, otherwise a named list of such
#'   vectors. A prime number yields \code{1}, and 1 itself yields
#'   \code{integer(0)} - its only divisor is 1, which is \code{x} itself and
#'   therefore not a proper one.
#'
#' @examples
#'
#' divisors(786)
#'
#' divisors(c(145, 786))
#'
#' # the number of proper divisors
#' length(divisors(786))
#'
#' # a prime has only one, and this one is at the integer limit
#' divisors(.Machine$integer.max)
#'
#' @family number.theory
#' @concept number-theory
#' @concept numerical-methods
#' @export
divisors <- function(x) {

  # is.finite() before the whole-number test: Inf %% 1 is NaN, so the old
  # `any(x %% 1 != 0)` evaluated to NA and the surrounding `if` then failed
  # with "missing value where TRUE/FALSE needed" - a message about the
  # condition rather than about the argument.
  if (!is.numeric(x) || anyNA(x) || any(!is.finite(x)) ||
      any(x < 1) || any(x != floor(x)))
    stop("'x' must contain positive whole numbers only.")

  # The limit stays where it is. Trial division runs to sqrt(x), so even the
  # largest permitted value costs at most 46341 iterations - there is no
  # reason to lower the bound now that the algorithm no longer allocates
  # x / 2 integers first.
  if (any(x > .Machine$integer.max))
    stop("'x' must not exceed .Machine$integer.max (", .Machine$integer.max, ").")

  res <- setNamesX(lapply(x, divs_cpp), x)

  if (length(x) == 1L)
    res[[1L]]
  else
    res
}
