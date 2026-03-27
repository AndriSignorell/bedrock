
#' Check Whether a Vector Is Dichotomous
#'
#' Determines whether a vector contains at most two distinct values.
#'
#' @param x A vector.
#' @param strict Logical. If \code{TRUE}, exactly two distinct values must
#'   be present. If \code{FALSE} (default), at most two distinct values are allowed.
#' @param na.rm Logical. If \code{TRUE}, missing values are removed before
#'   evaluation. If \code{FALSE} (default), the presence of \code{NA} results
#'   in \code{FALSE}.
#'
#' @return A single logical value.
#'
#' @examples
#' isDichotomous(c(0, 1, 1))
#' isDichotomous(c(1, 1, 1))
#' isDichotomous(c(1, 1, 1), strict = TRUE)
#' isDichotomous(c(0, 1, NA), na.rm = TRUE)
#' isDichotomous(c("A", "A", "B"))
#' isDichotomous(c("A", "A", "B", "C"))
#' isDichotomous(factor(c("A", "A", "B", "C")))


#' @export
isDichotomous <- function(x,
                          strict = FALSE,
                          na.rm = FALSE) {
  
  if (na.rm) {
    x <- x[!is.na(x)]
  } else if (anyNA(x)) {
    return(FALSE)
  }
  
  # empty vector handling
  if (length(x) == 0)
    return(!strict)
  
  n_unique <- length(unique(x))
  
  if (strict) {
    n_unique == 2L
  } else {
    n_unique <= 2L
  }
}
