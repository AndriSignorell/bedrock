
#' Recycle a List of Elements
#'
#' This function recycles all supplied elements to the maximal dimension.
#'
#' If \code{maxdim} is smaller than the length of an element, that element
#' is truncated to the first \code{maxdim} values. Zero-length elements are
#' recycled to \code{NA} vectors of length \code{maxdim}. Both situations
#' are rejected when \code{strict = TRUE}.
#'
#' @param maxdim defines the maximal dimension, if set to \code{NULL} (default)
#' the maximal dimension of the list.
#' @param strict logical, if \code{TRUE} each element must have length 1 or
#' \code{maxdim}, so that no partial recycling (or truncation) can occur.
#' Default is \code{FALSE}.
#' @param \dots a number of vectors of elements.
#'
#' @return a list of the supplied elements\cr \code{attr(,"maxdim")} contains
#' the maximal dimension of the recycled list.
#'
#' @seealso \code{\link{rep}}, \code{\link{replicate}}
#'
#' @keywords utilities
#' @examples
#'
#' recycle(x=1:5, y=1, s=letters[1:2])
#'
#' z <- recycle(x=letters[1:5], n=2:3, sep=c("-"," "))
#' sapply(1:attr(z, "maxdim"), function(i) paste(rep(z$x[i], times=z$n[i]),
#'                                         collapse=z$sep[i]))
#'
#' @family pkg.args
#' @concept programming
#' @concept introspection
#' @export
recycle <- function(..., maxdim = NULL, strict = FALSE) {

  lst  <- list(...)

  # --- empty input --------------------------------------------

  if (length(lst) == 0) {
    res <- list()
    attr(res, "maxdim") <- 0L
    return(res)
  }

  lens <- lengths(lst)

  # --- resolve maxdim --------------------------------------

  if (is.null(maxdim)) {
    maxdim <- max(lens)
  } else {
    if (!is.numeric(maxdim) || length(maxdim) != 1 ||
        is.na(maxdim) || maxdim <= 0 || maxdim %% 1 != 0)
      stop("'maxdim' must be a single positive whole number")
  }

  # --- strict check ------------------------------------------

  if (strict && !all(lens %in% c(1, maxdim))) {
    stop("Arguments must have length 1 or maxdim.")
  }

  # --- recycling ---------------------------------------------

  # rep(length.out =) instead of rep_len(), as it dispatches S3 methods
  # and therefore keeps classes like Date intact also in older R versions
  res <- lapply(lst, function(x) rep(x, length.out = maxdim))

  attr(res, "maxdim") <- maxdim
  return(res)
}
