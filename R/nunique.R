#' Count Unique Values
#'
#' Returns the number of unique elements in a vector.
#'
#' @param x A vector.
#' @param na.rm Logical. Should missing values (`NA`) be removed before
#'   counting unique values? Defaults to `FALSE`.
#'
#' @return An integer of length one.
#'
#' @examples
#' nunique(c(1, 1, 2, 3))
#'
#' nunique(c(1, 1, 2, NA))
#'
#' nunique(c(1, 1, 2, NA), na.rm = TRUE)
#'

#' @export
nunique <- function(x, na.rm = FALSE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  
  length(unique(x))
}
