

#' Inverse Which
#'
#' Reconstructs the \code{TRUE} positions from the index vector returned
#' by \code{\link{which}}, producing a logical vector of length \code{n}.
#' Note that this is not a perfect inverse: \code{which()} discards
#' \code{NA} and \code{FALSE} positions, so the original vector cannot be
#' fully recovered.
#'
#' Negative indices follow standard R semantics: \code{unwhich(-2, 5)}
#' returns a vector with \code{TRUE} everywhere \emph{except} position 2.
#' Positive and negative indices must not be mixed.
#'
#' @param idx     A vector of non-zero whole-number indices.  Positive
#'   values mark \code{TRUE} positions; negative values mark
#'   \code{FALSE} positions (all others become \code{TRUE}).  As in base
#'   R, positive and negative indices must not be mixed.  Duplicate
#'   indices are allowed and result in a single \code{TRUE} (or
#'   \code{FALSE}) at that position.
#' @param n       A single non-negative whole number giving the length of
#'   the result.  For positive \code{idx}, defaults to \code{max(idx)};
#'   for negative or empty \code{idx}, defaults to \code{0L}.  Must not
#'   be less than \code{max(abs(idx))}.
#' @param useNames Logical.  If \code{TRUE} (default) \emph{and}
#'   \code{idx} has names, those names are attached to the corresponding
#'   \code{TRUE} positions of the result; all other positions receive an
#'   empty string.  If \code{FALSE} or \code{idx} is unnamed, the result
#'   has no names.  Ignored for negative indices.
#'
#' @return A logical vector of length \code{n}.
#'
#' @note
#' Based on code by Nick Sabbe.
#'
#' @references
#' Sabbe, N. (2012). Inverse of \code{which}.
#'   \url{https://stackoverflow.com/questions/7659833/inverse-of-which}
#'
#' @seealso \code{\link{which}}
#'
#' @examples
#' ll <- c(TRUE, FALSE, TRUE, NA, FALSE, FALSE, TRUE)
#' names(ll) <- letters[seq_along(ll)]
#' i <- which(ll)
#'
#' # reconstruct TRUE positions (names preserved on TRUE positions)
#' unwhich(i, length(ll))
#'
#' # without names
#' unwhich(i, length(ll), useNames = FALSE)
#'
#' # negative index: TRUE everywhere except position 2
#' unwhich(-2, 5)
#'
#' # empty index -> all-FALSE vector
#' unwhich(integer(0), n = 5L)
#'
#' @family vector.ops
#' @concept vector-manipulation
#' @concept data-manipulation
#'


#' @export
unwhich <- function(idx,
                    n = if (length(idx) > 0L && all(idx > 0L)) max(idx) else 0L,
                    useNames = TRUE) {
  
  has_idx <- length(idx) > 0L
  
  # --- validate n ------------------------------------------------------
  if (!is.numeric(n) || length(n) != 1L || is.na(n) ||
      n < 0L || n != as.integer(n))
    stop("Argument 'n' must be a single non-negative whole number.")
  n <- as.integer(n)
  
  # --- validate idx ----------------------------------------------------
  if (has_idx) {
    if (!is.numeric(idx) || anyNA(idx) || any(idx == 0L) ||
        any(idx != as.integer(idx)))
      stop("Argument 'idx' must contain non-zero whole numbers only.")
    idx <- as.integer(idx)
    
    if (any(idx > 0L) && any(idx < 0L))
      stop("Argument 'idx' must not mix positive and negative indices.")
    
    if (n < max(abs(idx)))
      stop(gettextf(
        "Argument 'n' (%d) must not be less than max(abs(idx)) (%d).",
        n, max(abs(idx))
      ))
  }
  
  # --- build result ----------------------------------------------------
  res <- logical(n)
  
  if (has_idx) {
    if (all(idx > 0L)) {
      # positive: mark listed positions TRUE
      res[idx] <- TRUE
      if (useNames && !is.null(names(idx))) {
        nm      <- rep.int("", n)
        nm[idx] <- names(idx)
        names(res) <- nm
      }
    } else {
      # negative: all FALSE by default; set all except excluded to TRUE
      res[-idx] <- TRUE
    }
  }
  
  res
}

