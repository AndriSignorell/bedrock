
#' Merge default arguments with user overrides
#'
#' Internal helper used to:
#' - merge defaults with user arguments
#' - remove forbidden argument names
#' - optionally warn if forbidden arguments were supplied
#'
#' @param defaults Named list of default arguments.
#' @param user Named list of user-supplied arguments.
#' @param forbidden Character vector of argument names that are not allowed.
#' @param warn Logical; whether to issue a warning if forbidden arguments are removed.
#'
#' @return A named list of merged arguments.
#'


#' @export
mergeArgs <- function(defaults,
                       user,
                       forbidden = NULL,
                       warn = TRUE) {
  
  if (is.null(user))
    return(defaults)
  
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
  
  # merge defaults with user arguments (user values override defaults)
  modifyList(defaults, user)
  
}



