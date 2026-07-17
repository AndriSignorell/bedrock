
#' Merge Default Arguments with User Overrides
#'
#' Helper used to merge defaults with user arguments, remove
#' forbidden argument names, and optionally warn if forbidden arguments
#' were supplied.
#'
#' User values override defaults of the same name. Unlike
#' \code{\link{modifyList}}, elements with the value \code{NULL} are
#' preserved (so that an explicit \code{NULL} can be passed on as an
#' argument value instead of silently deleting the entry).
#'
#' @param defaults named list of default arguments.
#' @param user named list of user-supplied arguments, or \code{NULL}.
#' @param forbidden character vector of argument names that are not allowed.
#' @param warn logical; whether to issue a warning if forbidden arguments are removed.
#'
#' @return a named list of merged arguments.
#'
#' @examples
#' mergeArgs(list(col = "black", lty = 1), list(col = "red"))
#'
#' # explicit NULL survives the merge
#' mergeArgs(list(col = "black"), list(col = NULL))
#'
#' @seealso [utils::modifyList()]
#' 
#' @family pkg.args
#' @concept programming
#' @concept introspection
#' @export
mergeArgs <- function(defaults,
                      user,
                      forbidden = NULL,
                      warn = TRUE) {

  if (is.null(user) || length(user) == 0L)
    return(defaults)

  if (is.null(names(user)) || !all(nzchar(names(user))))
    stop("'user' must be a fully named list")

  if (!is.null(forbidden)) {
    bad <- intersect(names(user), forbidden)

    if (length(bad) > 0) {
      if (warn) {
        warning(
          "Ignoring forbidden arguments: ",
          paste(bad, collapse = ", "),
          call. = FALSE
        )
      }
      user <- user[!names(user) %in% forbidden]
    }
  }

  # merge, preserving explicit NULL values (the list() wrapper prevents
  # the deletion that modifyList() would perform)
  for (nm in names(user)) {
    defaults[nm] <- list(user[[nm]])
  }

  defaults

}
