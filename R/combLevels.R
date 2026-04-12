
#' Combine Levels from Multiple Inputs
#'
#' Extracts and combines the levels from one or more vectors or factors.
#' Non-factor inputs are coerced to factors before extracting levels.
#'
#' @param ... One or more vectors or factors.
#' @param sort Logical; if \code{TRUE}, the resulting levels are sorted.
#' @param na Logical; if \code{TRUE}, \code{NA} is treated as a valid level
#'   (i.e., included in the result).
#'
#' @details
#' Each input is coerced to a factor (if not already one), and its levels
#' are extracted. The union of all levels is returned.
#'
#' By default, missing values (\code{NA}) are not included as a level.
#' Set \code{na = TRUE} to include them.
#'
#' The order of levels follows their first occurrence unless
#' \code{sort = TRUE}.
#'
#' @return
#' A character vector containing the unique levels across all inputs.
#'
#' @examples
#' x <- factor(c("A", "B"))
#' y <- c("B", "C")
#'
#' combLevels(x, y)
#'
#' # Sorted levels
#' combLevels(x, y, sort = TRUE)
#'
#' # Including NA as a level
#' x <- c("A", NA)
#' y <- c("B", NA)
#' combLevels(x, y, na = TRUE)
#'


#' @export
combLevels <- function(..., sort = FALSE, na = FALSE) {
  
  dots <- list(...)
  
  if (length(dots) == 0L)
    return(character())
  
  lvl <- unlist(lapply(dots, function(x) {
    if (!inherits(x, "factor")) {
      x <- if (na) factor(x, exclude = NULL) else factor(x)
    }
    levels(x)
  }))
  
  lvl <- unique(lvl)
  
  if (sort) lvl <- sort(lvl)
  
  lvl
}

