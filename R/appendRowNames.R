
#' Append Rownames to a Data Frame or Matrix
#'
#' Adds the row names of a data.frame or matrix as a column.
#'
#' @param x A data.frame or matrix.
#' @param col_name Name of the new column containing the row names.
#' @param after Position after which the column is inserted.
#'   Default is 0 (first column).
#' @param remove_rownames Logical; if TRUE, existing row names are removed.
#'
#' @return A data.frame (or matrix coerced to data.frame) with row names
#'   added as a column.
#'
#' @seealso \code{\link{appendX}}
#'
#' @examples
#' dd <- data.frame(x=1:5, y=6:10, z=LETTERS[1:5],
#'                  row.names = letters[1:5])
#' appendRowNames(dd)
#'


#' @family data.manipulation
#' @concept data-manipulation
#'
#'
#' @export
appendRowNames <- function(x, col_name = "rowname",
                           after = 0L, remove_rownames = TRUE) {
  
  if (!(is.data.frame(x) || is.matrix(x)))
    stop("'x' must be a data.frame or matrix")
  
  rn <- rownames(x)
  if (is.null(rn))
    rn <- seq_len(nrow(x))
  
  res <- appendX(x, rn, after = after, names = col_name)
  
  if (remove_rownames)
    rownames(res) <- NULL
  
  res
}


