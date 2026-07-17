
#' Combine Levels from Multiple Inputs
#'
#' Extracts and combines the levels from one or more vectors or factors.
#' Non-factor inputs are coerced to factors before extracting levels.
#'
#' Each input is coerced to a factor (if not already one), and its levels
#' are extracted. The union of all levels is returned. Unused levels of
#' factor inputs are preserved.
#'
#' By default, missing values (\code{NA}) are not included as a level.
#' Set \code{na = TRUE} to include them; \code{NA} is then placed last
#' when sorting.
#'
#' The order of levels follows their first occurrence unless
#' \code{sorted = TRUE}.
#'
#' @param ... one or more vectors or factors.
#' @param sorted logical; if \code{TRUE}, the resulting levels are sorted.
#' @param na logical; if \code{TRUE}, \code{NA} is treated as a valid level
#'   (i.e., included in the result).
#'
#' @return
#' a character vector containing the unique levels across all inputs.
#'
#' @examples
#' x <- factor(c("A", "B"))
#' y <- c("B", "C")
#'
#' combLevels(x, y)
#'
#' # Sorted levels
#' combLevels(x, y, sorted = TRUE)
#'
#' # Including NA as a level
#' x <- c("A", NA)
#' y <- c("B", NA)
#' combLevels(x, y, na = TRUE)
#' 
#' 
#'
#' @family data.recode
#' @concept categorization
#' @concept recoding
#' @export
combLevels <- function(..., sorted = FALSE, na = FALSE) {

  dots <- list(...)

  if (length(dots) == 0L)
    return(character())

  lvl <- unlist(lapply(dots, function(x) {
    if (is.factor(x)) {
      # keep the existing levels (including unused ones) and append
      # NA if requested and present in the data
      lv <- levels(x)
      if (na && anyNA(x))
        lv <- c(lv, NA)
      lv
    } else {
      levels(factor(x, exclude = if (na) NULL else NA))
    }
  }))

  lvl <- unique(lvl)

  if (sorted)
    lvl <- sort(lvl, na.last = TRUE)

  lvl
}
