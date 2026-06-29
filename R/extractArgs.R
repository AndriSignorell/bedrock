

#' Extract named arguments from dots with defaults
#'
#' Utility to extract a subset of arguments from a list (typically `list(...)`)
#' and merge them with default values.
#'
#' @param dots Named list of arguments (usually `list(...)`)
#' @param defaults Named list of default values
#' @param validate Optional validation function
#' @param returnRest Logical; return unused arguments
#'
#' @return Named list of extracted arguments (and optionally remaining ones)



#' @family pkg.introspection  
#' @concept programming
#'
#'
#' @export
extractArgs <- function(dots, defaults, validate = NULL, returnRest = FALSE) {
  
  out <- defaults
  
  if (!is.null(names(dots))) {
    idx <- intersect(names(dots), names(defaults))
    if (length(idx)) {
      out[idx] <- dots[idx]
    }
  }
  
  if (!is.null(validate)) {
    validate(out)
  }
  
  if (returnRest) {
    rest <- dots[setdiff(names(dots), names(defaults))]
    return(list(args = out, rest = rest))
  }
  
  out
}

