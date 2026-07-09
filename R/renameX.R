
#' Rename Elements of a Named Object
#'
#' Renames selected elements of a named object by specifying old-to-new name
#' mappings.  Works on any R object that supports \code{\link{names}()},
#' including vectors, lists, data frames, and matrices.  For matrix-like
#' objects, \code{rownames} and \code{colnames} can be targeted via the
#' \code{which} argument.
#'
#' @details
#' The function supports three modes:
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
#'   \item{Function mode}{
#'     If a single function is passed in \code{...}, it is applied to all
#'     current names.  Useful for bulk transformations such as
#'     \code{toupper}, \code{tolower}, or \code{make.names}.}
#' }
#'
#' When \code{...} contains unnamed character elements, the names are assigned
#' positionally: the first element replaces \code{names(x)[1]}, the second
#' \code{names(x)[2]}, and so on.
#'
#' @param x     A named object.  Any type that supports \code{names()},
#'   \code{rownames()}, or \code{colnames()}, e.g. a vector, list, data frame,
#'   or matrix.
#' @param ...   Name mappings of the form \code{old = "new"}, a single
#'   function to apply to all names (e.g. \code{toupper}), or unnamed
#'   character strings applied positionally (see Details).
#' @param on Character scalar specifying which names to operate on.
#'   One of \code{"names"} (default), \code{"rownames"}, or \code{"colnames"}.
#'   Partial matching is supported.
#' @param useGsub Logical scalar.  If \code{TRUE}, each mapping is applied as
#'   a \code{gsub()} pattern substitution across all current names rather than
#'   an exact replacement.  Default is \code{FALSE}.
#' @param fixed  Logical scalar.  Passed to \code{\link{gsub}()} when
#'   \code{useGsub = TRUE}.  If \code{TRUE} (default), patterns are treated as
#'   fixed strings rather than regular expressions.
#' @param warn   Logical scalar.  If \code{TRUE} (default), a warning is
#'   issued when one or more old names supplied in \code{...} are not found in
#'   the targeted names of \code{x}.  Only relevant in exact mode
#'   (\code{useGsub = FALSE}).
#'
#' @return The object \code{x} with updated names; all other attributes are
#'   preserved.
#'
#' @seealso \code{\link{names}}, \code{\link{setNamesX}}
#'
#' @examples
#' x <- c(a = 1, b = 2, c = 3)
#'
#' # Exact mode: rename by old = "new" pairs
#' renameX(x, a = "alpha", c = "gamma")
#'
#' # Positional mode: replaces names(x)[1:2]
#' renameX(x, "alpha", "beta")
#'
#' # Function mode: apply a function to all names
#' renameX(x, toupper)
#' renameX(x, tolower)
#'
#' # Data frame columns
#' d <- data.frame(foo = 1:3, bar = 4:6)
#' renameX(d, foo = "x", bar = "y")
#'
#' # Pattern mode: strip a common prefix
#' y <- c(v_mean = 1, v_sd = 2, v_n = 3)
#' renameX(y, v_ = "", useGsub = TRUE)
#'
#' # Pattern mode with regex (fixed = FALSE)
#' renameX(y, `^v_` = "", useGsub = TRUE, fixed = FALSE)
#'
#' # Matrix: rename colnames selectively
#' m <- matrix(1:6, nrow = 2,
#'             dimnames = list(c("row_a", "row_b"), c("col_x", "col_y", "col_z")))
#' renameX(m, col_x = "alpha", on = "colnames")
#'
#' # Matrix: uppercase all rownames via function mode
#' renameX(m, toupper, on = "rownames")
#'
#' # Matrix: rename rownames via gsub
#' renameX(m, `row_` = "", useGsub = TRUE, fixed = FALSE, on = "rownames")
#'
#' @family data.manipulation
#' @concept attribute
#' @export
renameX <- function(x, ..., on = "names",
                    useGsub = FALSE, fixed = TRUE, warn = TRUE) {
  
  on <- match.arg(on, c("names", "rownames", "colnames"))
  
  # --- extract the targeted names -------------------------------------------
  nms <- switch(on,
                names    = names(x),
                rownames = rownames(x),
                colnames = colnames(x)
  )
  
  if (is.null(nms))
    stop(sprintf("Argument 'x' has no %s.", on))
  
  # --- early exit if nothing to do ------------------------------------------
  dots <- list(...)
  if (length(dots) == 0L)
    return(x)
  
  # --- function mode: single function in ... --------------------------------
  if (length(dots) == 1L && is.function(dots[[1L]])) {
    nms <- dots[[1L]](nms)
    return(.assignNames(x, nms, on))
  }
  
  if (any(vapply(dots, is.function, logical(1))))
    stop("A function must be the only argument in '...'.")

  subst <- c(...)

  # --- guard: mixed named/unnamed arguments are ambiguous ------------------
  argNms    <- names(subst)
  allNamed  <- all(nzchar(argNms))
  noneNamed <- all(!nzchar(argNms))
  if (!(allNamed || noneNamed))
    stop("Either all or none of the replacement arguments in '...' must be named.")
  
  # --- positional mode: replaces names(x)[1:length(subst)] -----------------
  if (noneNamed) {
    if (length(subst) > length(nms))
      stop("More replacement names supplied than names exist in 'x'.")
    nms[seq_along(subst)] <- subst
    return(.assignNames(x, nms, on))
  }
  
  # --- pattern mode (gsub over all names) -----------------------------------
  if (useGsub) {
    for (i in seq_along(subst))
      nms <- gsub(names(subst)[i], subst[i], nms, fixed = fixed)
    return(.assignNames(x, nms, on))
  }
  
  # --- exact mode (match old names, replace with new) ----------------------
  i <- match(names(subst), nms)
  
  if (anyNA(i)) {
    if (warn)
      warning("The following names were not found and were ignored: ",
              paste(names(subst)[is.na(i)], collapse = ", "))
    subst <- subst[!is.na(i)]
    i     <- i[!is.na(i)]
  }
  
  if (length(i))
    nms[i] <- subst
  
  .assignNames(x, nms, on)
}


# internal helper: write nms back to the correct slot of x
.assignNames <- function(x, nms, on) {
  switch(on,
         names    = { names(x)    <- nms },
         rownames = { rownames(x) <- nms },
         colnames = { colnames(x) <- nms }
  )
  x
}

