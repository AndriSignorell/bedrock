
#' Reverse Coding of Variables
#'
#' Reverses the coding of a vector. Supports numeric, logical, and factor inputs:
#' \itemize{
#'   \item **Numeric**: Transforms values using `min + max - x`
#'   \item **Logical**: Flips TRUE/FALSE
#'   \item **Factor**: Reverses the order of levels
#' }
#'
#' @param x a vector (numeric, logical, or factor).
#' @param min optional numeric minimum. Must be provided together with `max`.
#'   If `NULL` (default), the observed minimum of `x` is used.
#' @param max optional numeric maximum. Must be provided together with `min`.
#'   If `NULL` (default), the observed maximum of `x` is used.
#' @param na.rm logical; whether to ignore `NA`s when computing the range
#'   (numeric only). If `FALSE` and `NA`s are present, a warning is
#'   issued and `NA` is returned for all values. Default is `FALSE`.
#'
#' @return a vector of the same type and length as `x`, with reversed coding.
#'
#' @section Errors:
#' Throws an error if all values are `NA`, if only one of `min`/`max`
#' is provided, if `min > max`, or if `x` is not numeric, logical, or factor.
#' A warning is issued if values of `x` lie outside an explicitly
#' provided `[min, max]` range.
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




#' @family data.recode
#' @concept recoding
#' @concept categorization
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
      if (any(x < min | x > max, na.rm = TRUE)) {
        warning("Some values of `x` lie outside [min, max]; ",
                "the reversed values will fall outside the scale.")
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



