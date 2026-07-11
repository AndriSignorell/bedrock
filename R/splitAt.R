
#' Split a Vector at Given Positions
#'
#' Splits a vector into consecutive segments at specified positions.
#'
#' @param x a vector to be split
#' @param pos an integer vector of positions at which to split \code{x}.
#'   Positions refer to indices in \code{x} where a new segment should start.
#'
#' @return A list of vectors, each representing a segment of \code{x}.
#'
#' @details
#' The function splits \code{x} into consecutive chunks defined by \code{pos}.
#' Internally, positions are sorted, duplicates are removed, and values that
#' would not produce a non-empty segment (\code{pos < 2} or
#' \code{pos > length(x)}) are ignored. Empty segments are never returned.
#'
#' Each element of the returned list corresponds to a contiguous subset of
#' \code{x}. The first segment always starts at position 1.
#'
#' @examples
#' x <- 1:10
#'
#' # split at positions 4 and 7
#' splitAt(x, c(4, 7))
#'
#' # unsorted and duplicate positions are handled
#' splitAt(x, c(7, 4, 4, 20))
#'
#' @seealso \code{\link{split}}
#'




#' @family data.reshape
#' @concept reshape
#' @concept data-inspection
#' @export
splitAt <- function(x, pos) {

  if (!is.numeric(pos) || anyNA(pos) || any(pos %% 1 != 0))
    stop("'pos' must contain whole numbers only.")

  n <- length(x)

  if (n == 0L)
    return(list(x))

  pos <- sort(unique(as.integer(pos)))
  # pos = 1 would create an empty leading segment; suppress it, like
  # positions outside the valid range
  pos <- pos[pos >= 2L & pos <= n]

  idx <- c(1L, pos, n + 1L)

  Map(function(i, j) x[i:(j - 1L)],
      head(idx, -1L), tail(idx, -1L))

}

