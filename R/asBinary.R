
#' Coerce a Vector to Binary (0/1)
#'
#' A unified conversion utility for binary variables. Converts a logical,
#' numeric, factor, or character vector to a binary numeric vector coded
#' as 0 and 1.
#'
#' For \strong{logical} input, \code{TRUE} is mapped to 1 and \code{FALSE}
#' to 0.
#'
#' For \strong{numeric} input, values must already be 0 or 1 (or \code{NA});
#' any other value raises an error.
#'
#' For \strong{factor} input, the vector must have exactly two levels. By
#' default the second level is coded as 1. Use \code{pos} to specify which
#' level should be coded as 1.
#'
#' For \strong{character} input, the vector must have exactly two distinct
#' non-missing values. By default the alphabetically second value is coded
#' as 1. The same \code{pos} logic applies.
#'
#' @param x a logical, numeric, integer, factor, or character vector
#' @param pos optional positive value. If supplied, observations equal to
#'   \code{pos} are coded as 1 and all others as 0. Must be one of the
#'   observed values or factor levels.
#' @param warn logical. If \code{TRUE} (default), a warning is issued when
#'   a factor or character vector is coerced to binary without an explicit
#'   \code{pos}, indicating which value is coded as 1.
#'
#' @return a numeric vector of 0s and 1s (and \code{NA}s where present in
#'   \code{x}). For factor and character input, the result carries a
#'   \code{"coding"} attribute, a named integer vector documenting which
#'   original value was mapped to 0 and which to 1.
#'
#' @examples
#' # logical
#' asBinary(c(TRUE, FALSE, TRUE))
#'
#' # numeric (already binary)
#' asBinary(c(0, 1, 1, 0))
#'
#' # factor: second level coded as 1 by default
#' asBinary(factor(c("control", "treatment", "control")))
#'
#' # factor with explicit positive value
#' asBinary(factor(c("control", "treatment", "control")), pos = "treatment")
#'
#' # character
#' asBinary(c("no", "yes", "no", "yes"))
#'
#' # character with explicit positive value
#' asBinary(c("F", "U", "F", "U"), pos = "F")
#'
#' @family data.recode
#' @concept binary
#' @concept type-coercion
#' @export
asBinary <- function(x, pos = NULL, warn = TRUE) {

  ## -------------------------------------------------------------------
  ## logical
  ## -------------------------------------------------------------------
  if (is.logical(x))
    return(as.numeric(x))

  ## -------------------------------------------------------------------
  ## numeric / integer
  ## -------------------------------------------------------------------
  if (is.numeric(x)) {
    if (!all(x %in% c(0, 1, NA)))
      stop("numeric 'x' must contain only 0, 1, or NA")
    return(as.numeric(x))
  }

  ## -------------------------------------------------------------------
  ## factor
  ## -------------------------------------------------------------------
  if (is.factor(x)) {

    lev <- levels(x)

    if (length(lev) != 2L)
      stop("factor 'x' must have exactly 2 levels")

    if (is.null(pos)) {
      pos <- lev[2L]

      if (warn)
        warning(
          gettextf("coercing factor to binary (0/1): using '%s' as '1'", pos),
          call. = FALSE
        )

    } else if (!pos %in% lev) {
      stop("'pos' must be one of the factor levels: ",
           paste(lev, collapse = ", "))
    }

    result <- as.numeric(x == pos)
    attr(result, "coding") <- setNamesX(c(0L, 1L), c(setdiff(lev, pos), pos))

    return(result)

  }

  ## -------------------------------------------------------------------
  ## character
  ## -------------------------------------------------------------------
  if (is.character(x)) {

    u <- sort(unique(x[!is.na(x)]))

    if (length(u) != 2L)
      stop("character 'x' must have exactly 2 distinct non-missing values")

    if (is.null(pos)) {
      pos <- u[2L]

      if (warn)
        warning(
          gettextf("coercing character to binary (0/1): using '%s' as '1'", pos),
          call. = FALSE
        )

    } else if (!pos %in% u) {
      stop("'pos' must be one of the unique values: ",
           paste(u, collapse = ", "))
    }

    result <- as.numeric(x == pos)
    attr(result, "coding") <- setNamesX(c(0L, 1L), c(setdiff(u, pos), pos))

    return(result)

  }

  ## -------------------------------------------------------------------
  ## fallback
  ## -------------------------------------------------------------------
  stop("unsupported type for 'x': ", class(x)[1L])
}
