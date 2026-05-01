
#' Conditionally Call a Function
#'
#' Conditionally evaluate a function depending on the value of an argument.
#' This is a convenient helper for optional features such as plotting,
#' logging, or callbacks, where the user can enable, disable, or parameterize
#' a function call via a single argument.
#'
#' @param fun A function to be called.
#' @param arg Controls whether and how \code{fun} is called:
#'   \itemize{
#'     \item \code{FALSE}, \code{NULL}, or \code{NA}: \code{fun} is not called
#'       and \code{NULL} is returned invisibly.
#'     \item \code{TRUE}: \code{fun} is called with \code{defaults} (if provided),
#'       or with no arguments.
#'     \item A named list: \code{fun} is called with the list elements as arguments.
#'       If \code{defaults} is provided, it is merged with \code{arg}, where
#'       elements of \code{arg} override those in \code{defaults}.
#'   }
#' @param defaults A named list of default arguments passed to \code{fun} when
#'   \code{arg = TRUE}, or used as a base when \code{arg} is a list.
#'   Default is \code{NULL}.
#' @param forbidden Optional character vector of argument names that are not
#'   allowed. If any of these appear in \code{arg}, they are removed before
#'   calling \code{fun}. A warning is issued unless \code{warn = FALSE}.
#' @param warn Logical. If \code{TRUE} (default), a warning is issued when
#'   forbidden arguments are removed.
#'
#' @return Returns the result of \code{fun(...)} if called. If \code{arg} is
#'   \code{FALSE}, \code{NULL}, or \code{NA}, returns \code{NULL} invisibly.
#'
#' @details
#' This function implements a flexible pattern for optional function calls:
#'
#' \itemize{
#'   \item Enable/disable behavior with \code{TRUE}/\code{FALSE}
#'   \item Customize behavior with a list of arguments
#'   \item Provide safe defaults and restrict certain arguments
#' }
#'
#' The merging of \code{defaults} and \code{arg} is performed using
#' \code{\link{modifyList}}, where user-supplied arguments take precedence.
#'
#' @examples
#' # Simple usage: skip
#' callIf(message, FALSE)
#'
#' # Call with defaults
#' callIf(message, TRUE, defaults = list("Hello world"))
#'
#' # Call with explicit arguments
#' callIf(message, list(x = "Hello from callIf"))
#'
#' # With defaults + override
#' callIf(plot, list(x = 1:5),
#'        defaults = list(y = 1:5, type = "l"))
#'
#' # Forbid arguments
#' callIf(plot,
#'        list(x = 1:5, y = 1:5, col = "red"),
#'        forbidden = "col")
#'
#' # Typical use case: optional plotting
#' x <- 1:10
#' y <- x^2
#' callIf(plot, TRUE, defaults = list(x, y))
#'
#' @family utilities
#' @concept programming-helpers
#' @concept functional-programming
#'


#' @export
callIf <- function(fun, arg, defaults = NULL, forbidden = NULL, warn = TRUE) {
  
  if (isFALSE(arg) || is.null(arg) || isNA(arg))
    return(invisible(NULL))
  
  if (isTRUE(arg)) {
    args <- defaults %||% list()
    
  } else if (is.list(arg) && !is.null(names(arg))) {
    
    if (!is.null(forbidden)) {
      bad <- intersect(names(arg), forbidden)
      
      if (length(bad)) {
        msg <- sprintf(
          "Ignoring forbidden argument(s) for '%s': %s",
          deparse(substitute(fun)),
          paste(bad, collapse = ", ")
        )
        if (warn) warning(msg)
        arg[bad] <- NULL
      }
    }
    
    args <- if (is.null(defaults)) arg else modifyList(defaults, arg)
    
  } else {
    stop("Argument 'arg' must be TRUE, FALSE, NA/NULL or a named list.")
  }
  
  return(do.call(fun, args))
}

