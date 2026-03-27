
#' Split a Vector at Given Positions
#'
#' Splits a vector into consecutive segments at specified positions.
#'
#' @param x A vector to be split.
#' @param pos An integer vector of positions at which to split \code{x}.
#'   Positions refer to indices in \code{x} where a new segment should start.
#'
#' @return A list of vectors, each representing a segment of \code{x}.
#'
#' @details
#' The function splits \code{x} into consecutive chunks defined by \code{pos}.
#' Internally, positions are sorted, duplicates are removed, and values outside
#' the valid index range \code{[1, length(x)]} are ignored.
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



#' @export
splitAt <- function(x, pos) {
  
  n <- length(x)
  
  pos <- sort(unique(pos))
  pos <- pos[pos >= 1L & pos <= n]
  
  idx <- c(1L, pos, n + 1L)
  
  Map(function(i, j) {
    if(i > (j - 1L)) x[0] else x[i:(j - 1L)]
  }, head(idx, -1L), tail(idx, -1L))
  
}