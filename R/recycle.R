
#' Recycle a List of Elements
#'
#' This function recycles all supplied elements to the maximal dimension.
#'
#' If `maxDim` is smaller than the length of an element, that element
#' is truncated to the first `maxDim` values. Zero-length elements are
#' recycled to `NA` vectors of length `maxDim`. Both situations
#' are rejected when `strict = TRUE`.
#'
#' @param maxDim defines the maximal dimension, if set to `NULL` (default)
#' the maximal dimension of the list.
#' @param strict logical, if `TRUE` each element must have length 1 or
#' `maxDim`, so that no partial recycling (or truncation) can occur.
#' Default is `FALSE`.
#' @param \dots a number of vectors of elements.
#'
#' @return a list of the supplied elements\cr `attr(,"maxdim")` contains
#' the maximal dimension of the recycled list.
#'
#' @examples
#'
#' recycle(x=1:5, y=1, s=letters[1:2])
#'
#' z <- recycle(x=letters[1:5], n=2:3, sep=c("-"," "))
#' sapply(1:attr(z, "maxdim"), function(i) paste(rep(z$x[i], times=z$n[i]),
#'                                         collapse=z$sep[i]))
#'
#' @seealso [rep()], [replicate()]
#'
#' @family pkg.args
#' @concept programming
#' @concept introspection
#' @export
recycle <- function(..., maxDim = NULL, strict = FALSE) {

  lst  <- list(...)

  # --- empty input --------------------------------------------

  if (length(lst) == 0) {
    res <- list()
    attr(res, "maxdim") <- 0L
    return(res)
  }

  lens <- lengths(lst)

  # --- resolve maxdim --------------------------------------

  if (is.null(maxDim)) {
    maxDim <- max(lens)
  } else {
    if (!is.numeric(maxDim) || length(maxDim) != 1 ||
        is.na(maxDim) || maxDim <= 0 || maxDim %% 1 != 0)
      stop("'maxDim' must be a single positive whole number")
  }

  # --- strict check ------------------------------------------

  if (strict && !all(lens %in% c(1, maxDim))) {
    stop("Arguments must have length 1 or maxdim.")
  }

  # --- recycling ---------------------------------------------

  # rep(length.out =) instead of rep_len(), as it dispatches S3 methods
  # and therefore keeps classes like Date intact also in older R versions
  res <- lapply(lst, function(x) rep(x, length.out = maxDim))

  attr(res, "maxdim") <- maxDim
  return(res)
}
