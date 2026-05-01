
#' Reverse Coding of Variables
#'
#' Reverses the coding of a vector. Supports numeric, logical, and factor inputs:
#' \itemize{
#'   \item \strong{Numeric}: Transforms values using \code{min + max - x}
#'   \item \strong{Logical}: Flips TRUE/FALSE
#'   \item \strong{Factor}: Reverses the order of levels
#' }
#'
#' @param x A vector (numeric, logical, or factor).
#' @param min Optional numeric minimum. Must be provided together with \code{max}.
#'   If \code{NULL} (default), the observed minimum of \code{x} is used.
#' @param max Optional numeric maximum. Must be provided together with \code{min}.
#'   If \code{NULL} (default), the observed maximum of \code{x} is used.
#' @param na.rm Logical; whether to ignore \code{NA}s when computing the range
#'   (numeric only). If \code{FALSE} and \code{NA}s are present, a warning is
#'   issued and \code{NA} is returned for all values. Default is \code{FALSE}.
#'
#' @return A vector of the same type and length as \code{x}, with reversed coding.
#'
#' @section Errors:
#' Throws an error if all values are \code{NA}, if only one of \code{min}/\code{max}
#' is provided, if \code{min > max}, or if \code{x} is not numeric, logical, or factor.
#'
#' @examples
#' # Numeric
#' revCode(c(1, 2, 3, 4, 5))
#'
#' # Numeric with explicit range (e.g., Likert scale)
#' revCode(c(1, 2, 3, 4, 5), min = 1, max = 5)
#'
#' # Numeric with NAs
#' revCode(c(1, 2, NA, 4, 5), na.rm = TRUE)
#'
#' # Logical
#' revCode(c(TRUE, FALSE, TRUE))
#'
#' # Factor
#' x <- factor(c("low", "medium", "high"), ordered = TRUE)
#' revCode(x)
#'



#' @family data.manipulation
#' @concept data-manipulation
#' @concept factor-handling
#'
#'
#' @export
revCode <- function(x, min = NULL, max = NULL, na.rm = FALSE) {
  
  # ---- Numeric ----
  if (is.numeric(x)) {
    
    if (all(is.na(x))) {
      stop("All values are NA.")
    }
    
    # Genau eines von min/max angegeben → Fehler
    if (xor(is.null(min), is.null(max))) {
      stop("Either both `min` and `max` must be provided, or neither.")
    }
    
    if (!is.null(min) && !is.null(max)) {
      if (!is.numeric(min) || !is.numeric(max)) {
        stop("`min` and `max` must be numeric.")
      }
      if (min > max) {
        stop("`min` must be <= `max`.")
      }
      return(min + max - x)
    }
    
    # Kein min/max: aus Daten ableiten
    if (!na.rm && anyNA(x)) {
      warning("NAs present and `na.rm = FALSE`: returning NA for all values. Set `na.rm = TRUE` or provide `min`/`max` explicitly.")
    }
    
    rng <- range(x, na.rm = na.rm)
    return(sum(rng) - x)
  }
  
  # ---- Logical ----
  if (is.logical(x)) {
    if (all(is.na(x))) stop("All values are NA.")
    return(!x)
  }
  
  # ---- Factor ----
  if (is.factor(x)) {
    
    lv <- levels(x)
    
    if (length(lv) < 2) return(x)
    
    new_lv <- rev(lv)
    
    return(factor(new_lv[match(x, lv)], levels = new_lv, ordered = is.ordered(x)))
  }
  
  # ---- Unsupported ----
  stop("Unsupported type: must be numeric, logical, or factor.")
}



