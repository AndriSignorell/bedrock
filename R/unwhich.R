

#' Inverse Which
#'
#' Reconstructs the `TRUE` positions from the index vector returned
#' by [which()], producing a logical vector of length `n`.
#' Note that this is not a perfect inverse: `which()` discards
#' `NA` and `FALSE` positions, so the original vector cannot be
#' fully recovered.
#'
#' Negative indices follow standard R semantics: `unwhich(-2, 5)`
#' returns a vector with `TRUE` everywhere *except* position 2.
#' Positive and negative indices must not be mixed.
#'
#' @param idx     a vector of non-zero whole-number indices.  Positive
#'   values mark `TRUE` positions; negative values mark
#'   `FALSE` positions (all others become `TRUE`).  As in base
#'   R, positive and negative indices must not be mixed.  Duplicate
#'   indices are allowed and result in a single `TRUE` (or
#'   `FALSE`) at that position.
#' @param n       a single non-negative whole number giving the length of
#'   the result.  For positive `idx`, defaults to `max(idx)`;
#'   for negative or empty `idx`, defaults to `0L`.  Must not
#'   be less than `max(abs(idx))`.
#' @param useNames logical.  If `TRUE` (default) *and*
#'   `idx` has names, those names are attached to the corresponding
#'   `TRUE` positions of the result; all other positions receive an
#'   empty string.  If `FALSE` or `idx` is unnamed, the result
#'   has no names.  Ignored for negative indices.
#'
#' @return a logical vector of length `n`.
#'
#' @note
#' The positive-index construction (`rv[indices] <- TRUE` with name
#' propagation) is based on code by Nick Sabbe; negative-index handling
#' and input validation are original additions.
#'
#' @references
#' Sabbe, N. (2012). Inverse of `which`.
#'   <https://stackoverflow.com/questions/7659833/inverse-of-which>
#'
#' @seealso [which()]
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



#' @family vector.utils  
#' @concept indexing
#' @concept data-inspection
#'
#'
#' @export
unwhich <- function(idx,
                    n = if (length(idx) > 0L && !anyNA(idx) && all(idx > 0L)) max(idx) else 0L,
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
    # as.integer() would strip names, which useNames relies on
    storage.mode(idx) <- "integer"
    
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
      # negative: R's negative indexing selects "all except" directly
      res[idx] <- TRUE
    }
  }
  
  res
}

