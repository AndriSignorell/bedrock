
#' Check Whether a Vector Is Dichotomous
#'
#' Determines whether a vector contains at most two distinct values.
#'
#' @param x a vector
#'
#' @param strict logical. If \code{TRUE}, exactly two distinct values must
#'   be present. If \code{FALSE} (default), at most two distinct values are allowed.
#'
#' @param na.rm logical. If \code{TRUE}, missing values are removed before
#'   evaluation. If \code{FALSE} (default), the presence of \code{NA} results
#'   in \code{NA} (indeterminate status).
#'
#' @return \code{TRUE}, \code{FALSE}, or \code{NA} if the status cannot be
#'   determined because of missing values (see \code{na.rm}).
#'
#' @examples
#' isDichotomous(c(0, 1, 1))
#' isDichotomous(c(1, 1, 1))
#' isDichotomous(c(1, 1, 1), strict = TRUE)
#' isDichotomous(c(0, 1, NA))               # NA
#' isDichotomous(c(0, 1, NA), na.rm = TRUE)
#' isDichotomous(c("A", "A", "B"))
#' isDichotomous(c("A", "A", "B", "C"))
#' isDichotomous(factor(c("A", "A", "B", "C")))
#'
#' @family data.predicate
#' @concept type-test
#' @concept binary
#' @export
isDichotomous <- function(x, strict = FALSE, na.rm = FALSE) {

  if (anyNA(x)) {
    if (!na.rm)
      return(NA)
    x <- x[!is.na(x)]
  }

  if (length(x) == 0)
    return(!strict)

  nUnique <- length(unique(x))

  if (strict)
    nUnique == 2L
  else
    nUnique <= 2L

}
