
#' Get a single argument from dots with default
#'
#' Lightweight helper to extract a named argument from a list (typically list(...)).
#'
#' @param dots Named list (usually list(...))
#' @param name Character string, argument name
#' @param default Default value if argument not present
#'
#' @return The value of the argument or default


#' @export
getDotsArg <- function(dots, name, default = NULL) {
  
  if (!is.character(name) || length(name) != 1L)
    stop("name must be a single character string")
  
  if (!is.null(names(dots)) && name %in% names(dots)) {
    return(dots[[name]])
  }
  
  default
}

