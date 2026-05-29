
#' Rename Elements of a Named Object
#'
#' Renames selected elements of a named object by specifying old-to-new name
#' mappings.  Works on any R object that supports \code{\link{names}()},
#' including vectors, lists, data frames, and matrices.
#'
#' @details
#' The function supports two modes controlled by \code{useGsub}:
#'
#' \describe{
#'   \item{Exact mode (\code{useGsub = FALSE}, default)}{
#'     Names are matched exactly via \code{\link{match}()}.  Each element of
#'     \code{...} must be a named scalar character string of the form
#'     \code{old = "new"}.  Unmatched old names trigger a warning when
#'     \code{warn = TRUE}.}
#'   \item{Pattern mode (\code{useGsub = TRUE})}{
#'     Each mapping is treated as a \code{\link{gsub}()} substitution applied
#'     in sequence to \emph{all} current names.  The left-hand side is the
#'     pattern, the right-hand side is the replacement.  The \code{fixed}
#'     argument is forwarded to \code{gsub()}.}
#' }
#'
#' When \code{...} contains unnamed elements, the names are assigned
#' positionally: the first element replaces \code{names(x)[1]}, the second
#' \code{names(x)[2]}, and so on.
#'
#' @param x      A named object.  Any type that supports \code{names()},
#'   e.g. a vector, list, or data frame.
#' @param ...    Name mappings of the form \code{old = "new"}.  Unnamed
#'   elements are applied positionally (see Details).
#' @param useGsub Logical scalar.  If \code{TRUE}, each mapping is applied as
#'   a \code{gsub()} pattern substitution across all current names rather than
#'   an exact replacement.  Default is \code{FALSE}.
#' @param fixed  Logical scalar.  Passed to \code{\link{gsub}()} when
#'   \code{useGsub = TRUE}.  If \code{TRUE} (default), patterns are treated as
#'   fixed strings rather than regular expressions.
#' @param warn   Logical scalar.  If \code{TRUE} (default), a warning is
#'   issued when one or more old names supplied in \code{...} are not found in
#'   \code{names(x)}.  Only relevant in exact mode (\code{useGsub = FALSE}).
#'
#' @return The object \code{x} with updated names; all other attributes are
#'   preserved.
#'
#' @seealso \code{\link{names}}
#'
#' @family topic.utilities
#' @concept names
#' @concept rename
#' @concept data-manipulation
#'
#' @examples
#' # Exact mode: rename by old = "new" pairs
#' x <- c(a = 1, b = 2, c = 3)
#' renameX(x, a = "alpha", c = "gamma")
#'
#' # Positional mode: no names supplied in ...
#' renameX(x, "alpha", "beta")
#'
#' # Data frame columns
#' d <- data.frame(foo = 1:3, bar = 4:6)
#' renameX(d, foo = "x", bar = "y")
#'
#' # Pattern mode: strip a common prefix
#' y <- c(v_mean = 1, v_sd = 2, v_n = 3)
#' renameX(y, v_ = "", useGsub = TRUE)
#'
#' # Pattern mode with regex (fixed = FALSE): backticks needed for non-syntactic names
#' renameX(y, `^v_` = "", useGsub = TRUE, fixed = FALSE)
#'


#' @export
renameX <- function(x, ..., useGsub = FALSE, fixed = TRUE, warn = TRUE) {
  
  if (is.null(names(x)))
    stop("Argument 'x' has no names.")
  
  subst <- c(...)
  
  # --- guard: mixed named/unnamed arguments are ambiguous ---------------
  nms        <- names(subst)
  all_named  <- all(nzchar(nms))
  none_named <- all(!nzchar(nms))
  if (!(all_named || none_named))
    stop("Either all or none of the replacement arguments in '...' must be named.")
  
  # --- positional fallback: no names on ... → assign by position --------
  if (none_named) {
    if (length(subst) > length(names(x)))
      stop("More replacement names supplied than names exist in 'x'.")
    names(x)[seq_along(subst)] <- subst
    return(x)
  }
  
  # --- pattern mode (gsub over all names) --------------------------------
  if (useGsub) {
    nms <- names(x)
    for (i in seq_along(subst))
      nms <- gsub(names(subst)[i], subst[i], nms, fixed = fixed)
    names(x) <- nms
    return(x)
  }
  
  # --- exact mode (match old names, replace with new) -------------------
  i <- match(names(subst), names(x))
  
  if (anyNA(i)) {
    if (warn)
      warning("The following names were not found and were ignored: ",
              paste(names(subst)[is.na(i)], collapse = ", "))
    subst <- subst[!is.na(i)]
    i     <- i[!is.na(i)]
  }
  
  if (length(i))
    names(x)[i] <- subst
  
  x
}

