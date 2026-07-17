
#' Set the length of a vector, padding or truncating as needed
#'
#' Extends \code{x} to length \code{n} by appending \code{fill}, or
#' truncates it to length \code{n} if it is already longer -- like
#' \code{length(x) <- n}, but with a configurable fill value instead
#' of \code{NA}.
#'
#' @param x a vector.
#' @param n target length, a single non-negative whole number.
#' @param fill value used for newly added elements when \code{x} is
#'   extended (default \code{NA}).
#'
#' @return \code{x}, of length \code{n}.
#'
#' @examples
#' setLength(LETTERS[1:3], 5)
#' setLength(LETTERS[1:3], 2)
#' setLength(1:4, 6, fill = 0)
#'


#' @family vector.reshape
#' @concept reshape
#' @concept data-inspection
#' @export
setLength <- function(x, n, fill = NA) {

  if (length(n) != 1L || is.na(n) || n %% 1 != 0 || n < 0)
    stop("'n' must be a single non-negative whole number.")

  head(c(x, rep(fill, max(0, n - length(x)))), n)
}

