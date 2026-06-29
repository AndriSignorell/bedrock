

#' Extended str() with numbered variables
#'
#' Wrapper around [utils::str()] that optionally numbers variables in
#' lists and data frames. Useful for large objects where variables should
#' be referenced by position.
#'
#' By default, only top-level elements are numbered. Recursive numbering
#' of nested list elements can be enabled with `recursive = TRUE`.
#'
#' @param object Any R object.
#' @param ... Additional arguments passed to [utils::str()].
#' @param enumerate Logical. Should variables/elements be numbered?
#'   Default is `TRUE`.
#' @param recursive Logical. Should nested list elements also be numbered?
#'   Default is `FALSE`.
#' @param strict.width Character string passed to [utils::str()].
#'   Default is `"cut"`.
#'
#' @return
#' Invisibly returns the character vector produced by [utils::str()].
#'
#' @examples
#' # Data frame
#' strX(mtcars)
#'
#' # Nested list
#' x <- list(
#'   a = 1,
#'   b = list(
#'     c = 2,
#'     d = 3
#'   )
#' )
#'
#' strX(x)
#'
#' # Recursive numbering
#' strX(x, recursive = TRUE)
#'


#' @family pkg.introspection  
#' @concept introspection
#'
#'
#' @export
strX <- function(
    object,
    ...,
    enumerate = TRUE,
    recursive = FALSE,
    strict.width = "cut"
) {
  
  # ---- checks --------------------------------------------------------------
  
  stopifnot(
    is.logical(enumerate),
    length(enumerate) == 1L,
    is.logical(recursive),
    length(recursive) == 1L
  )
  
  # ---- capture str output --------------------------------------------------
  
  out <- capture.output(
    str(
      object,
      ...,
      strict.width = strict.width
    )
  )
  
  # ---- numbering -----------------------------------------------------------
  
  if (
    enumerate &&
    (is.list(object) || is.data.frame(object))
  ) {
    
    idx <- if (recursive) {
      grep("^\\s*\\$", out)
    } else {
      grep("^ \\$ ", out)
    }
    
    if (length(idx)) {
      
      width <- nchar(length(idx))
      
      out[idx] <- mapply(
        FUN = function(line, i) {
          
          num <- sprintf(
            paste0("%", width, "d"),
            i
          )
          
          sub(
            "^(\\s*)\\$",
            paste0("\\1", num, " $"),
            line
          )
        },
        line = out[idx],
        i = seq_along(idx),
        USE.NAMES = FALSE
      )
    }
  }
  
  # ---- print ---------------------------------------------------------------
  
  cat(out, sep = "\n")
  
  invisible(out)
}

