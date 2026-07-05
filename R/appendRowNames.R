
#' Append Rownames to a Data Frame or Matrix
#'
#' Adds the row names of a data.frame or matrix as a column.
#'
#' @param x A data.frame or matrix.
#' @param colName Name of the new column containing the row names.
#' @param after Position after which the column is inserted.
#'   Default is 0 (first column).
#' @param removeRowNames Logical; if TRUE, existing row names are removed.
#'
#' @return An object of the same class as \code{x} with the row names added
#'   as a column. Note that for matrices the result is coerced to the common
#'   mode, so appending (character) row names to a numeric matrix yields a
#'   character matrix.
#'
#' @seealso \code{\link{appendX}}, \code{\link{append}}
#'
#' @examples
#' dd <- data.frame(x = 1:5, y = 6:10, z = LETTERS[1:5],
#'                  row.names = letters[1:5])
#' appendRowNames(dd)
#'
#' @family data.manipulation
#' @concept table
#' @export
appendRowNames <- function(x, colName = "rowname",
                           after = 0L, removeRowNames = TRUE) {

  if (!(is.data.frame(x) || is.matrix(x)))
    stop("'x' must be a data.frame or matrix")

  rn <- rownames(x)
  if (is.null(rn))
    rn <- seq_len(nrow(x))

  res <- appendX(x, rn, after = after, newNames = colName)

  if (removeRowNames)
    rownames(res) <- NULL

  res
}
