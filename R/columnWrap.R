
#' Column Wrap
#'
#' Wraps text in a character matrix so that it's displayed over more than one
#' line.
#'
#' A data.frame containing character columns with long texts is often wrapped
#' by columns. This can lead to a loss of overview. `columnWrap()` wraps the
#' lines within the columns.
#'
#' @param x a character vector, typically one row of a matrix
#'   (e.g. via \code{apply(m, 1, columnWrap)})
#' @param width integer, the width of the columns in characters, recycled to
#'   the length of \code{x}. Defaults to an equal share of
#'   \code{getOption("width")} per column.
#'
#' @return a character matrix with one column per element of \code{x} and
#'   one row per wrapped line
#'
#' @seealso [strwrap()]
#' @keywords print
#' @examples
#'
#' print(columnWrap("This is a very long text for a table", 12))
#'
#' @family data.print
#' @concept formatting
#' @concept string-manipulation
#' @export
columnWrap <- function(x, width = NULL) {

  if (is.null(width)) {
    width <- getOption("width") / length(x)
  }

  width <- rep(width, length.out = length(x))

  lst <- lapply(seq_along(x), function(i) strwrap(x[[i]], width = width[i]))

  maxdim <- max(unlist(lapply(lst, length)))
  lst <- lapply(lst, function(z) c(z, rep("", maxdim - length(z))))

  do.call(cbind, lst)

}
