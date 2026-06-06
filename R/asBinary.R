
#' Coerce a Vector to Binary (0/1)
#'
#' Converts a logical, numeric, factor, or character vector to a binary
#' numeric vector coded as 0 and 1.
#'
#' @description
#' A unified conversion utility for binary variables. Handles the four most
#' common input types and provides consistent behavior across all of them.
#'
#' For \strong{logical} input, \code{TRUE} is mapped to 1 and \code{FALSE}
#' to 0.
#'
#' For \strong{numeric} input, values must already be 0 or 1 (or \code{NA});
#' any other value raises an error.
#'
#' For \strong{factor} input, the vector must have exactly two levels. By
#' default the second level (alphabetically) is coded as 1. Use \code{ref}
#' to specify which level should be coded as 1.
#'
#' For \strong{character} input, the vector must have exactly two distinct
#' non-missing values. The same \code{ref} logic applies.
#'
#' @param x a logical, numeric, integer, factor, or character vector.
#' @param ref optional reference value. If supplied, observations equal to
#'   \code{ref} are coded as 1 and all others as 0. Must be one of the
#'   observed values or factor levels.
#' @param warn logical. If \code{TRUE} (default), a warning is issued when
#'   a factor or character vector is coerced to binary without an explicit
#'   \code{ref}, indicating which value is coded as 1.
#'
#' @return a numeric vector of 0s and 1s (and \code{NA}s where present in
#'   \code{x}).
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
#' # factor with explicit reference
#' asBinary(factor(c("control", "treatment", "control")), ref = "treatment")
#'
#' # character
#' asBinary(c("no", "yes", "no", "yes"))
#'
#' # character with explicit reference
#' asBinary(c("F", "U", "F", "U"), ref = "F")
#'
#' @family data-utils
#' @concept data-manipulation



#' @export
asBinary <- function(x, ref = NULL, warn = TRUE) {
  
  x <- unname(x)
  
  ## -------------------------------------------------------------------
  ## logical
  ## -------------------------------------------------------------------
  if (is.logical(x))
    return(as.numeric(x))
  
  ## -------------------------------------------------------------------
  ## numeric / integer
  ## -------------------------------------------------------------------
  if (is.numeric(x) || is.integer(x)) {
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
    
    if (!is.null(ref)) {
      if (!ref %in% lev)
        stop("'ref' must be one of the factor levels: ",
             paste(lev, collapse = ", "))
      return(as.numeric(x == ref))
    }
    
    if (warn)
      warning(
        gettextf("coercing factor to binary (0/1): using '%s' as '1'", lev[2]),
        call. = FALSE
      )
    
    # factor-Zweig analog:
    result <- as.numeric(x == lev[2])
    attr(result, "coding") <- setNamesX(c(0L, 1L), lev)
    
    return(result)
    
  }
  
  ## -------------------------------------------------------------------
  ## character
  ## -------------------------------------------------------------------
  if (is.character(x)) {
    
    u <- sort(unique(x[!is.na(x)]))
    
    if (length(u) != 2L)
      stop("character 'x' must have exactly 2 distinct non-missing values")
    
    if (!is.null(ref)) {
      if (!ref %in% u)
        stop("'ref' must be one of the unique values: ",
             paste(u, collapse = ", "))
      return(as.numeric(x == ref))
    }
    
    if (warn)
      warning(
        gettextf("coercing character to binary (0/1): using '%s' as '1'", u[2]),
        call. = FALSE
      )
    
    result <- as.numeric(x == u[2])
    attr(result, "coding") <- setNames(c(0L, 1L), u)
    
    return(result)
    
  }
  
  ## -------------------------------------------------------------------
  ## fallback
  ## -------------------------------------------------------------------
  stop("unsupported type for 'x': ", class(x)[1L])
}

