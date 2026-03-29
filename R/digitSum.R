
#' Digit sum for integer values
#'
#' Computes the sum of digits for integer inputs. Negative values are handled
#' by taking the absolute value.
#'
#' @param x An integer vector.
#'
#' @return An integer vector containing the digit sums.
#'
#' @details
#' The function only accepts integer values. If non-integer numerics are supplied,
#' an error is thrown. Missing values (\code{NA}) are propagated.
#'
#' @examples
#' digitSum(124L)
#' digitSum(c(10L, 99L, -1234L))
#'

#' @export
digitSum <- function(x) {
  # --- type check ---
  if (!is.integer(x)) {
    stop("`x` must be an integer vector (e.g., use 1L instead of 1).", call. = FALSE)
  }
  
  # --- NA handling ---
  out <- integer(length(x))
  na_idx <- is.na(x)
  out[na_idx] <- NA_integer_
  
  # --- core computation ---
  out[!na_idx] <- vapply(abs(x[!na_idx]), function(z) {
    if (z == 0L) return(0L)
    
    sum(as.integer(strsplit(as.character(z), "")[[1]]))
  }, integer(1L))
  
  out
}