
#' Check a Vector For Being Zero
#'
#' Test if x is zero. This is done by checking if the numeric value is
#' below the machine tolerance.
#'
#' @param x a (non-empty) numeric or complex vector of data values.
#' @param tol tolerance to be used.
#' @param na.rm logical, indicating whether `NA` values should be stripped
#' before the computation proceeds. Defaults to `FALSE`.
#'
#' @return logical vector of the same length as x (after optional
#' `NA` removal). Non-numeric input yields all-`FALSE`.
#' 
#' @references
#' Burns, P. (2011). _The R Inferno_.
#' <https://www.burns-stat.com/documents/books/the-r-inferno/>
#'
#' @examples
#' # "... These are people who live in ignorance of the Floating Point Gods.
#' # These pagans expect [...] the following to be TRUE" (Burns, 2011):
#' (.1 - .3 / 3) == 0
#'
#' # they might be helped by
#' isZero(.1 - .3 / 3)
#'
#' @seealso [all.equal()]
#'
#' @family data.predicate
#' @concept type-test
#' @concept data-inspection
#' @export
isZero <- function(x, tol = sqrt(.Machine$double.eps), na.rm = FALSE) {

  if (na.rm)
    x <- x[!is.na(x)]

  if (is.numeric(x) || is.complex(x))
    abs(x) < tol
  else
    rep(FALSE, length(x))

}
