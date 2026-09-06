
#' Conditionally Call a Function
#'
#' Conditionally evaluate a function depending on the value of an argument.
#' This is a convenient helper for optional features such as plotting,
#' logging, or callbacks, where the user can enable, disable, or parameterize
#' a function call via a single argument.
#'
#' This function implements a flexible pattern for optional function calls:
#'
#' \itemize{
#'   \item Enable/disable behavior with `TRUE`/`FALSE`
#'   \item Customize behavior with a list of arguments
#'   \item Provide safe defaults and restrict certain arguments
#' }
#'
#' When merging `defaults` and `arg`, user-supplied arguments take
#' precedence. Unlike [modifyList()], elements with the value
#' `NULL` are preserved and passed on to `fun` (so that an explicit
#' `NULL` can be used to reset an argument).
#'
#' @param fun a function to be called.
#' @param arg controls whether and how `fun` is called:
#'   \itemize{
#'     \item `FALSE`, `NULL`, or `NA`: `fun` is not called
#'       and `NULL` is returned invisibly.
#'     \item `TRUE`: `fun` is called with `defaults` (if provided),
#'       or with no arguments.
#'     \item a fully named list: `fun` is called with the list elements as
#'       arguments. If `defaults` is provided, it is merged with
#'       `arg`, where elements of `arg` override those in
#'       `defaults`.
#'   }
#' @param defaults a named list of default arguments passed to `fun` when
#'   `arg = TRUE`, or used as a base when `arg` is a list.
#'   Default is `NULL`.
#' @param forbidden optional character vector of argument names that are not
#'   allowed. If any of these appear in `arg`, they are removed before
#'   calling `fun`. A warning is issued unless `warn = FALSE`.
#' @param warn logical. If `TRUE` (default), a warning is issued when
#'   forbidden arguments are removed.
#'
#' @return returns the result of `fun(...)` if called. If `arg` is
#'   `FALSE`, `NULL`, or `NA`, returns `NULL` invisibly.
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
#' @family pkg.args
#' @concept programming
#' @export
callIf <- function(fun, arg, defaults = NULL, forbidden = NULL, warn = TRUE) {

  if (isFALSE(arg) || is.null(arg) || isNA(arg))
    return(invisible(NULL))

  if (isTRUE(arg)) {
    args <- defaults %||% list()

  } else if (is.list(arg)) {

    if (is.null(names(arg)) || !all(nzchar(names(arg))))
      stop("'arg' must be a fully named list.")

    if (!is.null(forbidden)) {
      bad <- intersect(names(arg), forbidden)
      if (length(bad)) {
        if (warn)
          warning(
            gettextf("Ignoring forbidden argument(s) for '%s': %s",
                     deparse(substitute(fun)),
                     paste(bad, collapse = ", ")),
            call. = FALSE
          )
        arg[bad] <- NULL
      }
    }

    args <- defaults %||% list()
    for (nm in names(arg))
      args[nm] <- list(arg[[nm]])   # list() wrapper preserves NULL values

  } else {
    stop("Argument 'arg' must be TRUE, FALSE, NA/NULL or a named list.")
  }

  return(do.call(fun, args))
}
