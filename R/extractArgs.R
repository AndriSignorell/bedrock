
#' Extract Named Arguments from Dots with Defaults
#'
#' Utility to extract a subset of arguments from a list (typically
#' \code{list(...)}) and merge them with default values. Elements of
#' \code{dots} override entries in \code{defaults} of the same name;
#' explicit \code{NULL} values are preserved.
#'
#' @param dots named list of arguments (usually \code{list(...)})
#' @param defaults named list of default values
#' @param validate optional validation function, called with the merged
#'   argument list for its side effect. It should throw an error on
#'   invalid input; its return value is ignored.
#' @param returnRest logical; if \code{TRUE}, a list with components
#'   \code{args} (the merged arguments) and \code{rest} (all elements of
#'   \code{dots} not matching a default, including unnamed ones) is
#'   returned
#'
#' @return Named list of extracted arguments, or a list with components
#'   \code{args} and \code{rest} if \code{returnRest = TRUE}.
#'
#' @examples
#' dots <- list(col = "red", lwd = 2, 99)
#' extractArgs(dots, defaults = list(col = "black", lty = 1))
#'
#' extractArgs(dots, defaults = list(col = "black", lty = 1),
#'             returnRest = TRUE)
#'
#' @family pkg.args
#' @concept programming
#' @concept introspection
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
    # keep everything that does not match a default, including unnamed
    # elements (name-based subsetting would silently drop those)
    nm <- names(dots)
    if (is.null(nm)) nm <- rep("", length(dots))
    rest <- dots[!(nzchar(nm) & nm %in% names(defaults))]
    return(list(args = out, rest = rest))
  }

  out
}
